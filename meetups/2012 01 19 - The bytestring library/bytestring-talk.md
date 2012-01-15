A guided tour through the bytestring library
============================================

The purpose of the library
--------------------------

Provide datastructures for *efficiently* (both in time and space) computing
with (possibly infinite) sequences of bytes.


Why not `[Word8]`?
------------------

- Too many indirections: lots of pointer chasing, cache misses
- Too much space overhead (5 words for one byte): 
    * factor 19 on 32-bit systems
    * factor 39 on 64-bit systems

The representation of `[1,2] :: [Word8]` illustrates that nicely. Each box
represents one word.
~~~~
  +---------+-----+-----+       +---------+-----+-----+       +--------+
  | (:)-Tag | Ptr | Ptr | ----> | (:)-Tag | Ptr | Ptr | ----> | []-Tag |
  +---------+-----+-----+       +---------+-----+-----+       +--------+
               |                             |
               |                             |
               v                             v
         +------------+---+            +------------+---+
         | Word8#-Tag | 1 |            | Word8#-Tag | 2 |
         +------------+---+            +------------+---+
~~~~



Datastructures supported by the bytestring library
--------------------------------------------------

- Strict bytestrings

- Lazy bytestrings

- Lazy bytestring builder (new in 0.10.0.0, not yet on Hackage)

(http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects)

= Strict ByteStrings

~~~

data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length

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

~~~

= Slicing 1

~~~
take :: Int -> ByteString -> ByteString
take n ps@(PS x s l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}
~~~

= Slicing 2

~~~
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


= Creating strict bytestrings

~~~
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

= Strict ByteStrings Summary

~~~~

Pros
  - compact representation
  - simple 
  - slicing

Cons
  - expensive append
  - require exactly the space 
  - overhead per bytestring: 9 words (according to Johan Tibell)


~~~~


= Lazy ByteStrings

~~~
data ByteString = 
       Empty 
     | Chunk {-# UNPACK #-} !S.ByteString ByteString

~~~

= Interlude: Difference Lists 1/2


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

== A canonical example: in-order traversal of trees ==

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


= Difference Lists: 

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

== Side note: removing lazyness ==

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

= Lazy ByteString Difference List

~~~
import qualified Data.ByteString.Lazy as L

newtype LazyByteStringC = LazyByteStringC 
        { runLazyByteStringC :: L.ByteString -> L.ByteString }

instance Monoid LazyByteStringC where
  mempty = L

~~~

= Lazy ByteString Builder

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
