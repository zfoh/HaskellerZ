% A guided tour through the bytestring library
% Simon Meier
% HaskellerZ meetup - January 19th, 2012

What this talk covers
=====================

  - The [`bytestring`](http://hackage.haskell.org/package/bytestring) library
    - strict and lazy bytestrings
    - the new bytestring builder (to be included in version `0.10.0.0`,
       [repo](https://github.com/meiersi/bytestring),
       preview [docs](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public)
      )
    - their use cases, implementation, memory layout, and
      other stuff that's good to know for writing efficient code

  - Side-dishes
    - benchmarking with the
      [`criterion`](http://hackage.haskell.org/package/criterion) library
    - perhaps some [`vacuum`](http://hackage.haskell.org/package/vacuum-cairo)
      datastructure visualization

[talk repository](https://github.com/meiersi/HaskellerZ/tree/master/meetups/2012%2001%2019%20-%20The%20bytestring%20library):
  sources and [`pandoc`](http://johnmacfarlane.net/pandoc/index.html) 
  (what a pleasure to create slides™) config

Purpose of the bytestring library
=================================

Provide datastructures for computing *efficiently*, both in time and space,
with finite sequences and infinite streams of bytes.


Why not `[Word8]`?
==================

- Too many indirections: lots of pointer chasing,
  [cache misses](http://www.akkadia.org/drepper/cpumemory.pdf)
- Too much space overhead (5 machine words for one byte): 
    * i.e., a factor 19 (!) overhead on 32-bit systems
    * i.e., a factor 39 (!) overhead on 64-bit systems

See below for the actual memory representation of `[1,2] :: [Word8]`.
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
or the [paper on pointer-tagging](http://research.microsoft.com/en-us/um/people/simonpj/papers/ptr-tag/index.htm)
by [Simmon Marlow](http://research.microsoft.com/en-us/people/simonmar/), 
[Alexey Rodriguez
Yakushev](http://arodriguezyakushev.wordpress.com/publications/), and
[Simon Peyton Jones](http://research.microsoft.com/en-us/people/simonpj/).
[Johan Tibell](http://blog.johantibell.com/) also posted a good overview of the 
[memory overhead of common Haskell types](http://blog.johantibell.com/2011/06/memory-footprints-of-some-common-data.html).


Datastructures supported by the `bytestring` library
====================================================

  - strict bytestrings
    - a chunk of memory
    - use for finite sequences of bytes (e.g., result of parsing binary data)

  - lazy bytestrings
    - a lazy, linked list of chunks of memory
    - use for long sequences of bytes, possibly generated lazily

  - lazy bytestring builder 
    (new in 0.10.0.0, not yet on Hackage, 
     [repo](https://github.com/meiersi/bytestring),
     [docs](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public)
    )
    - a buffer filling function
    - supports *O(1)* `append` ⇒ efficient composition of short byte sequences
    - ensures large chunk sizes 
      (important to amortize overhead introduced by chunk boundaries)
    - use for implementing encodings, i.e., 
      conversion from Haskell values to sequences/streams of bytes


Strict bytestrings
==================

 - *Overhead* of a typical bytestring: 9 machine words.
 - uses pinned memory ⇒ pointers are stable, can be passed to C-APIs
 - `ForeignPtr`s are versatile: safely reference memory allocated by C-APIs
   (e.g., [memory mapped files](http://hackage.haskell.org/package/mmap))

~~~~ {.haskell }
-- A typical bytestring is of the form
exampleBS = PS (ForeignPtr addr (PlainPtr array) len off

-- Copied from "bytestring:Data.ByteString.Internal":
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length
~~~~

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





Slicing support
===============

  - slicing (storing offset and length) allows efficient `take` and `drop`
  - may result in memory leaks due to unintended sharing 
    (consider a 5-byte slice of a 32kb chunk of input)

~~~ {.haskell}
-- Assume the following qualified import
import qualified Data.ByteString      as S

take :: Int -> S.ByteString -> S.ByteString
take n ps@(PS fp off len)
    | n <= 0    = empty
    | n >= len  = ps
    | otherwise = PS fp off n
{-# INLINE take #-}
~~~

Implementation tricks
=====================


  - exploit mutable, unboxed datastructure using the
    [FFI](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html)
    ⇒ lots of `IO` code
    (only in internal code: the official API is *simple* and *pure*)
  - remove abstraction overhead through inlining


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

unsafeDrop :: Int -> ByteString -> ByteString
unsafeDrop n (PS x s l) = assert (0 <= n && n <= l) $ PS x (s+n) (l-n)
{-# INLINE unsafeDrop #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
~~~


Creating strict bytestrings
===========================

  - further performance tricks
    - use [bang
      patterns](http://www.haskell.org/ghc/docs/latest/html/users_guide/bang-patterns.html)
      to remove unnecessary laziness
    - exploit standard C-routines like `memset` and `memcpy`;
      they are highly optimized


~~~ {.haskell}
-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = PS nullForeignPtr 0 0

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton c = unsafeCreate 1 $ \p -> poke p c

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

Performance stumbling blocks
============================

 - repeated `append` **is slow** (quadratic cost) ⇒ use bytestring builders
 - `pack`ing and `unpack`ing from/to `[Word8]` is **expensive**
   ⇒ try to avoid this
 - use cases like (`pack . show`) are covered by efficient primitives 
   provided by the new bytestring builder
     - e.g.,
       [`intDec`](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public/Data-ByteString-Lazy-Builder-ASCII.html#v:intDec)
       efficently performs a decimal encoding of an `Int`

~~~ {.haskell}
pack :: [Word8] -> ByteString
pack ws = unsafePackLenBytes (List.length ws) ws

unsafePackLenBytes :: Int -> [Word8] -> ByteString
unsafePackLenBytes len xs0 =
    unsafeCreate len $ \p -> go p xs0
  where
    go !_ []     = return ()
    go !p (x:xs) = poke p x >> go (p `plusPtr` 1) xs
~~~

Strict bytestrings summary
==========================

  - Pros

    - compact representation of a sequence of bytes
    - pure, list-like API
    - support slicing
    - well-suited for interacting with C-APIs

  - Cons

    - expensive append
    - overhead per bytestring: 9 words (this is a lot for short bytestrings)
    - cannot free unused parts of their underlying chunk of memory


Lazy bytestrings
================

Just a lazy list of strict bytestrings.

~~~ {.haskell}
data ByteString = 
       Empty 
     | Chunk {-# UNPACK #-} !S.ByteString ByteString
~~~

  - efficient and compact represention of a pure stream of bytes
      - unreferenced chunks collected by GC
      - sometimes not expressive enough; errors cannot be reported

  - used, *but not recommended*, for lazy input from the OS
      - lazy reading of chunks ⇒ stream processing with constant memory
      - works for simple one-shot programs
      - for long-running programs a better means for dealing with exceptions
        and scarce resources like file descriptors is required 
        (see libraries like: 
        [`iteratee`](http://hackage.haskell.org/package/iteratee),
        [`enumerator`](http://hackage.haskell.org/package/enumerator),
        [`iterIO`](http://hackage.haskell.org/package/iterIO),
        [`conduit`](http://hackage.haskell.org/package/conduit), etc.)

  - repeated `append`s are **expensive** (quadratic cost, too small chunks)
    ⇒ use lazy bytestring builder

  - chunk boundaries incur a performance overhead
    ⇒ use lazy bytestrings conciously


Interlude: difference lists
===========================

- difference lists are a representation of sequences that supports an *O(1)*
  append (at the cost of a loss of sharing)
- provided by the [`dlist`](http://hackage.haskell.org/package/dlist) library,
  but often implemented directly
- a simple form of the [codensity
  transformation](http://comonad.com/reader/2011/free-monads-for-less/),
  explained well in
  [this blog post](http://blog.ezyang.com/2012/01/problem-set-the-codensity-transformation/)


A canonical use case for difference lists
=========================================

\begin{code}
-- strictness annotations
{-# LANGUAGE BangPatterns #-}

-- and a bunch of imports for our later benchmarks
import Data.Monoid (Monoid(..))
import Criterion.Main (defaultMain, nf, bgroup, bench)
\end{code}

A canonical use case for difference lists: in-order traversal of trees

\begin{code}
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving( Eq, Ord, Show )

fullTree :: Int -> Tree Int
fullTree n
  | n <= 0    = Leaf
  | otherwise = Node n t' t'
  where
    t' = fullTree (n - 1)

-- This version does not scale linearly with the number of nodes 
-- in the tree, as (++) is required to traverse some nodes repeatedly.
inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node x l r) = inorder l ++ ([x] ++ inorder r)

-- This version scales linearly, but is hard to read.
inorder' :: Tree a -> [a]
inorder' t = 
    go t []
  where
    go Leaf         rest = rest
    go (Node x l r) rest = go l (x : go r rest)
\end{code}

Recall the definition of `(++)`

~~~ {.haskell}
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
~~~



In-order traversal using difference lists 
=========================================

A `DList` is a function that, given the desired rest of the list, returns
the complete list.

\begin{code}
newtype DList a = DList { runDList :: [a] -> [a] }

singleton :: a -> DList a
singleton x = DList (x:)

toList :: DList a -> [a]
toList dl = runDList dl []

-- The standard API for the empty `DList` and concatenation
instance Monoid (DList a) where
   mempty            = DList id
   dl1 `mappend` dl2 = DList (runDList dl1 . runDList dl2)

-- This operator should really make it into base.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- much easier to read than `inorder'`
inorderDL :: Tree a -> [a]
inorderDL =
    toList . go
  where
    go Leaf         = mempty
    go (Node x l r) = go l <> singleton x <> go r
\end{code}

Note that [`Semigroups`](http://hackage.haskell.org/package/semigroups) are
even simpler than
[`Monoids`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html):
they provide only the "append" operation.
Interestingly, this suffices for many tasks.


Benchmarking using `criterion`
==============================

In GHCi, the `inorderDL` version is the slowest,
but benchmarks are worth nothing without turning optimizations on ;-)

The [talk](https://github.com/meiersi/HaskellerZ/tree/master/meetups/2012%2001%2019%20-%20The%20bytestring%20library)
is a literate Haskell file,
[cabal](http://www.haskell.org/cabal/)ized, and ready to run the following
[`criterion`](hackage.haskell.org/package/criterion) benchmarks.

\begin{code}
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

  - both `inorder'` and `indorderDL` are equally fast and scale linearly
  - `inorder` does not scale linearly. 
     It is 5 times slower than `inorderDL` for `fullTree 10` and 6.6 times
     slower for `fullTree 13`.

Side note: removing lazyness
============================

Note that a similar transform as for `inorder'` also speeds up the size
computation of `Tree`s. However, here we gain "only" a constant factor 2
speedup. It is due to the removing the unnecessary laziness in the size
computation.

\begin{code}
size :: Tree a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

-- this version is a factor 2x faster than `size`
size' :: Tree a -> Int
size' t0 = 
    go t0 0
  where
    go Leaf         !s = s
    go (Node _ l r) !s = go l (go r (s + 1))
\end{code}


The lazy bytestring builder
===========================

  - use for implementing encodings 
    (i.e., conversions from Haskell values to sequences of bytes)
      - e.g., HTML, HTTP response, JSON, CSV etc. generation
      - see this [example in the builder
        documentation](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public/Data-ByteString-Lazy-Builder.html)
  - design goals/features
    - provide efficient primitives for encoding standard Haskell values
    - *O(1)* append ⇒ fast composition of short sequences
    - ensure large average chunks sizes (for amortizing chunk boundary overhead)
    - indepent from buffer allocation strategy 
        - application can choose appriate strategy (e.g., avoid buffer
          allocation by executing directly on a `Handle`'s internal buffer)
    - allow full transfer of control over chunk creation
        - insert (large) strict and lazy bytestrings directly
        - efficiently switch to a different application-specific method for generating
          lazy bytestrings (e.g., parallel encoding for extra-low latency)
        - used in zero-copy algorithms for [size-prefixing and chunking of
          builders](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public/Data-ByteString-Lazy-Builder-Extras.html#g:8)

  - a redesigned/extended version of 
    [Jasper van der Jeugt](http://jaspervdj.be/)'s and 
    [Simon Meier](http://lambda-view.blogspot.com)'s
    [`blaze-builder`](http://hackage.haskell.org/package/blaze-builder)
    library (used for example in 
    [`yesod`](http://www.yesodweb.com/),
    [`snap`](http://snapframework.com/), and
    [`aeson`](http://hackage.haskell.org/package/aeson))


The types underlying the lazy bytestring builder
=================================================

~~~ {.haskell}
data BufferRange = BufferRange 
       {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
       {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

-- a lazy bytestring difference-list annotated with its size
data SizedChunks =
         SizedChunks {-# UNPACK #-} !Int64 (L.ByteString -> L.ByteString)

-- a buffer-range-filling function
type BuildStep a = BufferRange -> IO (BuildSignal a)

-- the result of filling a buffer-range
data BuildSignal a =
    Done 
      {-# UNPACK #-} !(Ptr Word8)     -- pointer to first byte after data
                     a                -- value computed (for Put monad)

  | BufferFull                        -- signal a full buffer
      {-# UNPACK #-} !Int             -- minimal size for next buffer
      {-# UNPACK #-} !(Ptr Word8)     -- pointer to first byte after data
                     !(BuildStep a)   -- next buildstep to call

  | InsertChunks                      -- transfer control over chunk generation
      {-# UNPACK #-} !(Ptr Word8)     -- pointer to first byte after data
      {-# UNPACK #-} !SizedChunks     -- produced chunks
                     !(Maybe Buffer)  -- partial buffer to be filled further
                     !(BuildStep a)   -- next buildstep to call

-- the lazy bytestring builder: a difference-list of buffer filling functions
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

-- the Put monad for computing a value while filling a buffer
-- (e.g., a checksum over the generated chunks or an error report)
newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }
~~~


How to construct primitive lazy bytestring builders
===================================================

  - use the [types and combinators](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public/Data-ByteString-Lazy-Builder-BasicEncoding.html)
    provided for
    - [*fixed-size encodings*](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public/Data-ByteString-Lazy-Builder-BasicEncoding.html#g:1),
      which always result in a sequence of bytes of a predetermined, fixed
      length
    - [*bounded-size
      encodings*](http://people.inf.ethz.ch/meiersi/downloads/private/bytestring-0.10.0.0-public/Data-ByteString-Lazy-Builder-BasicEncoding.html#g:4),
      which always result in a sequence of bytes that is no larger than a
      predetermined bound

  - combinators allow to fuse escaping with character encoding
    (e.g., combined escaping and UTF-8 encoding of HTML, JSON, or Haskell
    strings)
  - rewriting rules fuse buffer-free checks of consecutive primitive builders
  - useful for wrapping C-implementions like [V8's decimal encodings for IEEE
    floats](http://hackage.haskell.org/package/double-conversion)


An example of a bounded-size encoding: `int8Dec`
================================================

~~~ {.haskell}
-- | Decimal encoding of an 'Int8'.
{-# INLINE int8Dec #-}
int8Dec :: BoundedEncoding Int8
int8Dec = boundedEncoding 4 $ c_int_dec . fromIntegral

-- | An encoding that always results in sequence of bytes that is no longer
-- than a pre-determined bound.
data BoundedEncoding a = BE {-# UNPACK #-} !Int (a -> Ptr Word8 -> IO (Ptr Word8))

-- | Encode a value with a 'BoundedEncoding'.
{-# INLINE[1] encodeWithB #-}
encodeWithB :: BoundedEncoding a -> (a -> Builder)
encodeWithB (BE bound io) x =
    -- It is important to avoid recursive 'BuildStep's where possible, as
    -- their closure allocation is expensive. Using 'ensureFree' allows the
    -- 'step' to assume that at least 'sizeBound w' free space is available.
    ensureFree (bound) `mappend` builder step
  where
    step k (BufferRange op ope) = do
        op' <- io x op
        let !br' = BufferRange op' ope
        k br'

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'.
{-# INLINE ensureFree #-}
ensureFree :: Int -> Builder
ensureFree minFree =
    builder step
  where
    step k br@(BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ bufferFull minFree op k
      | otherwise                   = k br

-- fast decimal encoding of `CInt`s
foreign import ccall unsafe "static _hs_bytestring_int_dec" c_int_dec
    :: CInt -> Ptr Word8 -> IO (Ptr Word8)
~~~


HTML escaping fused with UTF-8 encoding
=======================================

~~~ {.haskell}
import qualified Data.ByteString.Lazy.Builder.BasicEncoding  as E
import           Data.ByteString.Lazy.Builder.BasicEncoding
                 ( ifB, fromF, (>*<), (>$<) )

{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: E.BoundedEncoding Char
charUtf8HtmlEscaped =
    ifB (>  '>' ) E.charUtf8 $  -- '>' is the largest escaped 'Char'
    ifB (== '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
    ifB (== '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
    ifB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
    ifB (== '"' ) (fixed5 ('&',('#',('3',('4',';'))))) $  -- &#34;
    (fromF E.char7)             -- fallback for remaining 'Char's
  where
    {-# INLINE fixed4 #-}
    fixed4 x = fromF $ const x >$<
      E.char7 >*< E.char7 >*< E.char7 >*< E.char7
     
    {-# INLINE fixed5 #-}
    fixed5 x = fromF $ const x >$<
      E.char7 >*< E.char7 >*< E.char7 >*< E.char7 >*< E.char7
~~~

 - The slightly awkward syntax is because the combinators are written such
   that the size-bound of the resulting `BoundedEncoding` can be
   computed at compile time.

 - The resulting code checks first, if there are 5 bytes free in the current
   buffer. If not, then it requests a new buffer. Otherwise, the character is
   encoded. Note the writing of the escaped characters is compiled down to
   the corresponding sequence of fixed `mov` operations. Nice.

Some missing pieces in our library infrastructure
=================================================

  - Use cases not covered well by any existing Haskell library
    - representing short sequences of bytes (slicing and `ForeignPtr` overhead
      too expensive)
    - very efficiently creating short bytestrings,
      i.e., strict bytestring builders
        - typical use-case: remote procedure calls
        - may support more efficient length-prefixing for
          Google's [protocol buffer
          encoding](http://code.google.com/apis/protocolbuffers/docs/encoding.html)

  - Use cases covered by other libraries
    - parsing binary data: use
      [`attoparsec`](http://hackage.haskell.org/package/attoparsec)
    - representing sequences of Unicode characters: use
      [`text`](http://hackage.haskell.org/package/text)
    - thanks to [Bryan O'Sullivan](http://www.serpentine.com/blog/) for
      creating these libraries
    - binary serialization: use 
      [`binary`](http://hackage.haskell.org/package/binary)
      (might be redesigned to make use of new bytestring builder features)


Conclusions
===========

  - use bytestrings to represent binary data
  - use the lazy bytestring builder to implement encodings
  - use `pack`, `unpack`, and `append` conciously 
    (when writing performance critical code)
  - ensure that the chunks of lazy bytestrings are large enough to amortize
    the chunk boundary overhead
  - *for writing very efficient code*: understand the memory layout and the
    cost of the operations of your types

**Enjoy Haskell**: 
enabling the construction of pure *and* efficient API's is just beautiful ☺

Thanks...
=========

...for listening. 

> Control.Monad.Has.Questions> ?
