# A guided tour through the new bytestring library

# The purpose of the library #

Provide datastructures for *efficiently* (both in time and space) computing
with (possibly infinite) sequences of bytes.


# Why not `[Word8]`?

- Too many indirections: lots of pointer chasing, cache misses
- Too much space overhead (5 words for one byte): 
    * factor 19 on 32-bit systems
    * factor 39 on 64-bit systems

The representation of `[1,2] :: [Word8]` illustrates that nicely. Each box
represents one word.
~~~
  +---------+-----+-----+       +---------+-----+-----+       +--------+
  | (:)-Tag | Ptr | Ptr | ----> | (:)-Tag | Ptr | Ptr | ----> | []-Tag |
  +---------+-----+-----+       +---------+-----+-----+       +--------+
               |                             |
               |                             |
               v                             v
         +------------+---+            +------------+---+
         | Word8#-Tag | 1 |            | Word8#-Tag | 2 |
         +------------+---+            +------------+---+
~~~

Let us have a look using vacuum-cairo.


# Datastructures supported by the bytestring library #

- Strict bytestrings

- Lazy bytestrings

- Lazy bytestring builder (new in 0.10.0.0, not yet on Hackage)

(http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects)

# 
