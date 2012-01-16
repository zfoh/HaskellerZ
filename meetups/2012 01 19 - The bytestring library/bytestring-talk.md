% A guided tour through the bytestring library
% Simon Meier
% January 19th, 2012

Purpose of the bytestring library
=================================

Provide datastructures for computing *efficiently*, both in time and space,
with (finite) sequences and (infinite) streams of bytes.


Why not `[Word8]`?
==================

- Too many indirections: lots of pointer chasing, cache misses
- Too much space overhead (5 machine words for one byte): 
    * i.e., a factor 19 overhead on 32-bit systems
    * i.e., a factor 39 overhead on 64-bit systems

See below for the actual manual representation of `[1,2] :: [Word8]`.
Each box represents one machine word.

~~~
  +----------+-----+-----+      +----------+-----+-----+      +---------+
  | (:)-Info | Ptr | Ptr | ---> | (:)-Info | Ptr | Ptr | ---> | []-Info |
  +----------+-----+-----+      +----------+-----+-----+      +---------+
               |                            |
               |                            |
               v                            v
         +-------------+---+          +-------------+---+
         | Word8#-Info | 1 |          | Word8#-Info | 2 |
         +-------------+---+          +-------------+---+
~~~

For more information on the heap layout of the GHC RTS see the 
[GHC commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects)
or the 2004 paper on pointer-tagging.


Datastructures supported by the bytestring library
==================================================

Strict bytestrings

  - a chunk of memory 
  - finite by construction
  - used for finite sequences of bytes (e.g., result of parsing binary data)

Lazy bytestrings

  - a lazy, linked list of chunks of memory
  - used for lazily generated sequences of bytes

Lazy bytestring builder 
  (new in 0.10.0.0, not yet on Hackage, 
  [repo](https://github.com/meiersi/bytestring),
  [docs](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public)
  )

  - a buffer filling function
  - supports *O(1)* append
  - ensures large chunk sizes 
    (important to amortize overhead introduced by chunk boundaries)
  - used for implementing encodings, i.e., 
    conversion from Haskell values to sequences/streams of bytes

*Missing:* Strict bytestring builder

  - might be interesting for implementing efficient length-prefixing in
    Google's [protocol buffer
    encoding](http://code.google.com/apis/protocolbuffers/docs/encoding.html)
    for short messages


Strict bytestrings
==================

~~~~ {.haskell }
-- Copied from "bytestring:Data.ByteString.Internal":

data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length

-- A typical bytestring is of the form
exampleBS = PS (ForeignPtr addr (PlainPtr array) len off
~~~~

*Overhead* of a typical bytestring: 9 machine words.

~~~~ {.haskell }
-- Copied from "base:GHC.ForeignPtr":

data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents

data Finalizers
  = NoFinalizers
  | CFinalizers
  | HaskellFinalizers
    deriving Eq

data ForeignPtrContents
  = PlainForeignPtr !(IORef (Finalizers, [IO ()]))
  | MallocPtr      (MutableByteArray# RealWorld) !(IORef (Finalizers, [IO ()]))
  | PlainPtr       (MutableByteArray# RealWorld)
~~~~




Strict bytestrings support slicing
==================================

  - slicing allows efficient `take` and `drop`
  - may result in memory leaks due to sharing 
    (consider: a 5-byte slice of a 32kb chunk of input)

~~~ {.haskell}
-- Assume the following qualified import
import qualified Data.ByteString      as S

take :: Int -> S.ByteString -> S.ByteString
take n ps@(PS x s l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}
~~~

More slicing examples
=====================

Key elements for performance

  - exploit unboxed datastructure using the FFI => lots of `IO` code
  - inline "wrappings" to remove abstraction overhead

~~~ {.haskell}
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = unsafeDrop (findIndexOrEnd (not . f) ps) ps
{-# INLINE dropWhile #-}

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Word8 -> Bool) -> ByteString -> Int
findIndexOrEnd k (PS x s l) = 
    inlinePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    STRICT2(go)
    go ptr n | n >= l    = return l
             | otherwise = do w <- peek ptr
                              if k w
                                then return n
                                else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}

unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n (PS x s l) = assert (0 <= n && n <= l) $ PS x (s+n) (l-n)
{-# INLINE unsafeDrop #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
~~~


Creating strict bytestrings
===========================

Further performance trick:

  - exploit standard C-routines like `memset` and `memcpy`.
    They are highly optimized.

Nevertheless: repeated `append` **is slow** => use bytestring builders

~~~ {.haskell}
-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = PS nullForeignPtr 0 0

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton c = unsafeCreate 1 $ \p -> poke p c

pack :: [Word8] -> ByteString
pack ws = unsafePackLenBytes (List.length ws) ws

unsafePackLenBytes :: Int -> [Word8] -> ByteString
unsafePackLenBytes len xs0 =
    unsafeCreate len $ \p -> go p xs0
  where
    go !_ []     = return ()
    go !p (x:xs) = poke p x >> go (p `plusPtr` 1) xs

replicate :: Int -> Word8 -> ByteString
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreate w $ \ptr ->
                      memset ptr c (fromIntegral w) >> return ()

append :: ByteString -> ByteString -> ByteString
append (PS _   _    0)    b                  = b
append a                  (PS _   _    0)    = a
append (PS fp1 off1 len1) (PS fp2 off2 len2) =
    unsafeCreate (len1+len2) $ \destptr1 -> do
      let destptr2 = destptr1 `plusPtr` len1
      withForeignPtr fp1 $ \p1 -> memcpy destptr1 (p1 `plusPtr` off1) len1
      withForeignPtr fp2 $ \p2 -> memcpy destptr2 (p2 `plusPtr` off2) len2
~~~


Strict ByteStrings Summary
==========================

~~~~
Pros

  - simple compact representation
  - supports slicing
  - works well for interacting with C-APIs

Cons

  - expensive append
  - require exactly the space 
  - overhead per bytestring: 9 words


~~~~


Lazy ByteStrings
================

Just a lazy list of strict bytestrings.

~~~ {.haskell}
data ByteString = 
       Empty 
     | Chunk {-# UNPACK #-} !S.ByteString ByteString
~~~

Mostly an intermediate representation

  - efficient represention for a pure stream of bytes
      * sometimes not expressive enough; errors cannot be reported

  - used, /but not recommended/, for lazy input from the OS
      * works for simple one-shot programs
      * for long-running programs a better means for dealing with scarce
        resources like file descriptors is required 
        (see libraries like: iteratee, enumerator, iterIO, conduit, etc.)

  - repeated `append`s are slow => use bytestring builders


Interlude: Difference Lists
===========================


Difference lists allow an O(1) append (at the cost of a loss of sharing).

  - See package `dlist`.
  - Blog entry: E. Z. Yang

Enable strictness annotations.

> {-# LANGUAGE BangPatterns #-}

A bunch of imports for later use.

\begin{code}
import Data.Monoid (Monoid(..))
import Criterion.Main (defaultMain, nf, bgroup, bench)
\end{code}

A canonical example: in-order traversal of trees

\begin{code}
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving( Eq, Ord, Show )

fullTree :: Int -> Tree Int
fullTree n
  | n <= 0    = Leaf
  | otherwise = Node n t' t'
  where
    t' = fullTree (n - 1)

inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

inorder' :: Tree a -> [a]
inorder' t = 
    go t []
  where
    go Leaf         rest = rest
    go (Node x l r) rest = go l (x : go r rest)

\end{code}


Difference Lists: 
=================

\begin{code}
newtype DList a = DList { runDList :: [a] -> [a] }

singleton :: a -> DList a
singleton x = DList (x:)

toList :: DList a -> [a]
toList dl = runDList dl []

instance Monoid (DList a) where
   mempty            = DList id
   dl1 `mappend` dl2 = DList (runDList dl1 . runDList dl2)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

inorderDL :: Tree a -> [a]
inorderDL =
    toList . go
  where
    go Leaf         = mempty
    go (Node x l r) = go l <> singleton x <> go r

main :: IO ()
main = defaultMain $ 
  [ bgroup "inorder" $ concatMap (\mkBench -> map mkBench depths) $
    [ \n -> bench ("inorder"   ++ show n) $ nf inorder   $ (fullTree n) 
    , \n -> bench ("inorder'"  ++ show n) $ nf inorder'  $ (fullTree n) 
    , \n -> bench ("inorderDL" ++ show n) $ nf inorderDL $ (fullTree n) 
    ]
  , bgroup "size" $ concatMap (\mkBench -> map mkBench depths) $
      [ \n -> bench ("size"      ++ show n) $ nf size      $ (fullTree n) 
      , \n -> bench ("size'"     ++ show n) $ nf size'     $ (fullTree n) 
      ]
  ]
  where
    depths = [10..13]
\end{code}

Side note: removing lazyness
============================

Note that a similar transform also speeds up the size computionat of `Tree`s.
However, here we "only" get a constant factor. It is due to the removed
unnecessary lazyness.

\begin{code}
size :: Tree a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

size' :: Tree a -> Int
size' t0 = 
    go t0 0
  where
    go Leaf         !s = s
    go (Node _ l r) !s = go l (go r (s + 1))
\end{code}

~~~

Lazy ByteString Difference List
===============================

~~~
import qualified Data.ByteString.Lazy as L

newtype LazyByteStringC = LazyByteStringC 
        { runLazyByteStringC :: L.ByteString -> L.ByteString }

instance Monoid LazyByteStringC where
  mempty = L

~~~

Lazy ByteString Builder
=======================

~~~
data BufferRange = BufferRange 
       {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
       {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

data SizedChunks =
         SizedChunks {-# UNPACK #-} !Int64 (L.ByteString -> L.ByteString)

type BuildStep a = BufferRange -> IO (BuildSignal a)

data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !(BuildStep a)
  | InsertChunks
      {-# UNPACK #-} !(Ptr Word8)
      {-# UNPACK #-} !SizedChunks
                     !(Maybe Buffer)  
                     !(BuildStep a)

newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }

~~~
