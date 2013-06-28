{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -Wall #-}

module HandsOn_Fuse_Tha_StreaaaamZ where

import           Data.List

{-

A Hands-On Introduction to Stream Fusion
========================================

References:

"Stream Fusion. From Lists to Streams to Nothing at All" (2007)
  by Duncan Coutts , Roman Leshchinskiy , Don Stewart
  http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401

"Exploiting Vector Instructions with Generalized Stream Fusion" (ICFP 2013)
  by Geoffrey Mainland, Roman Leshchinskiy, Simon Peyton Jones
  http://research.microsoft.com/en-us/um/people/simonpj/papers/ndp/haskell-beats-C.pdf



What's the problem?
-------------------

- compositional style results in readable code
- the resulting intermediate lists cost *lots* of performnance
   => fuse operations to get both readable AND performant code :-)

Context:
-}


map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs


sum' :: [Int] -> Int
sum' =
    go 0                                 -- sum'.1
  where
    go !acc []     = acc                 -- sum'.2
    go !acc (x:xs) = go (x + acc) xs     -- sum'.3

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' from to
  | from > to = []
  | otherwise = from : enumFromTo' (from + 1) to


-- fastAndClever n = (n + 1) * (n + 2) `div` 2

slow :: Int -> Int
slow n = sum' (map' (+1) (enumFromTo' 0 n ))






{-

Build/foldr fusion
-------------------

- built into GHC base
- does not work for fusing left-folds
- filter faces inefficencies
- no zips

-}




-- NO tail recursion
exampleBuildrFoldr :: Int -> Int
exampleBuildrFoldr n = foldr (+) 0 (map (+1) (enumFromTo 0 n))

-- NO full fusion (left-fold)
exampleBuildrFoldr' :: Int -> Int
exampleBuildrFoldr' n = foldl' (+) 0 (map (+1) (enumFromTo 0 n))

{-

Streams
-------

-}

data Step s a = Yield a s
              | Skip s
              | Done

data Stream a = forall s. Stream (s -> Step s a) !s

unstream :: Stream a -> [a]
unstream (Stream step s0) = go s0
  where
    go s = case step s of
      Done       -> []
      Skip s'    -> go s'
      Yield x s' -> x : go s'

stream :: [a] -> Stream a
stream xs0 = Stream step xs0
  where
    step []     = Done
    step (x:xs) = Yield x xs

-- | The sequence 1,1,1,...
onesS :: Stream Int
onesS = Stream (\_ -> Yield 1 ()) ()

enumFromToS :: Int -> Int -> Stream Int
enumFromToS from0 to = Stream step from0
  where
    step from | from > to = Done
              | otherwise = Yield from (from + 1)

mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream step s0) = Stream step' s0
  where
    step' s = case step s of
                Done       -> Done
                Skip s'    -> Skip s'
                Yield x s' -> Yield (f x) s'


sumS :: Stream Int -> Int
sumS (Stream step s0) = go 0 s0
  where
    go !acc s = case step s of
                  Done       -> acc
                  Skip s'    -> go acc       s'
                  Yield x s' -> go (x + acc) s'

----------------------------------------------------------------------------

{-
enumFromToS :: Int -> Int -> Stream Int
enumFromToS =
  \from0 to ->
      let step from | from > to = Done
                    | otherwise = Yield from (from + 1)
      in Stream step from0


sumS :: Stream Int -> Int
sumS =
  \stream -> case stream of
    Stream step s0 ->
      let go !acc s = case step s of
                        Done       -> acc
                        Yield x s' -> go (x + acc) s'
      in go 0 s0


fastS :: Int -> Int
fastS n = sumS (enumFromToS 0 n)

~~>

fastS n = (sumS) (enumFromToS 0 n)

~~>

  (\stream -> case stream of
    Stream step s0 ->
      let go !acc s = case step s of
                        Done       -> acc
                        Yield x s' -> go (x + acc) s'
      in go 0 s0
  )
  ( (\from0 to ->
      let step from | from > to = Done
                    | otherwise = Yield from (from + 1)
      in Stream step from0
    )
    0
    n
  )

~~>

  case
      let step from | from > n  = Done
                    | otherwise = Yield from (from + 1)
      in Stream step 0
  of
    Stream step s0 ->
      let go !acc s = case step s of
                        Done       -> acc
                        Yield x s' -> go (x + acc) s'
      in go 0 s0

~~>

  let step from | from > n  = Done
                | otherwise = Yield from (from + 1)
  in
    case Stream step 0 of
      Stream step s0 ->
        let go !acc s = case step s of
                          Done       -> acc
                          Yield x s' -> go (x + acc) s'
        in go 0 s0


~~>
  let go !acc from =
        case
          (if from > n then Done else Yield from (from + 1))
        of
          Done       -> acc
          Yield x s' -> go (x + acc) s'
  in go 0 0

~~>
  let go !acc from =
        case
          case from > n of
            True  -> Done
            False -> Yield from (from + 1))
        of
          Done       -> acc
          Yield x s' -> go (x + acc) s'
  in go 0 0

~~>
  let go !acc from =
        case from > n of
          True  -> case Done of
                     Done       -> acc
                     Skip s'    -> go acc       s'
                     Yield x s' -> go (x + acc) s'
          False -> case Yield from (from + 1)) of
                     Done       -> acc
                     Skip s'    -> go acc       s'
                     Yield x s' -> go (x + acc) s'
  in go 0 0

~~>
  let go !acc from = case from > n of
                       True  -> acc
                       False -> go (from + acc) (from + 1)
  in go 0 0
-}


filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream step s0) = Stream step' s0
  where
    step' s = case step s of
      Done                   -> Done
      Skip s'                -> Skip s'
      Yield x s' | p x       -> Yield x s'
                 | otherwise -> Skip s'


appendS :: forall a. Stream a -> Stream a -> Stream a
appendS (Stream stepL (sL0 :: sL))  (Stream stepR (sR0 :: sR)) =
    Stream step (Left sL0)
  where
    step :: Either sL sR -> Step (Either sL sR) a
    step (Left sL) = case stepL sL of
      Done        -> Skip (Right sR0)
      Skip sL'    -> Skip (Left sL')
      Yield x sL' -> Yield x (Left sL')

    step (Right sR) = case stepR sR of
      Done        -> Done
      Skip sR'    -> Skip (Right sR')
      Yield x sR' -> Yield x (Right sR')


{-


go :: Either a b -> Int -> Int

go (Left sL) acc = case stepL sL of
  Done        -> go (Right sR0) acc
  Skip sL'    -> go (Left sL')  acc
  Yield x sL' -> go (Left sL')  (acc + x)

go (Right sR) acc = case stepR sR of
  Done        -> acc
  Skip sR'    -> go (Right sR') acc
  Yield x sR' -> go (Right sR') (acc + x)


-----------------------------------------------------
go (Left sL)  = goLeft  sL
go (Right sR) = goRight sR


goLeft sL acc = case stepL sL of
  Done        -> goRight sR0 acc
  Skip sL'    -> goLeft sL'  acc
  Yield x sL' -> goLeft sL'  (acc + x)

goRight sR acc = case stepR sR of
  Done        -> acc
  Skip sR'    -> goRight sR' acc
  Yield x sR' -> goRight sR' (acc + x)

-}








{-
instance Monoid (Stream a) where
  mempty  = Stream (\_ -> Done) ()
  mappend = appendS

  mappe(Stream stepL sL0)  (Stream stepR sR0) = Stream step (Left sL0)
    where
      step :: Either sL sR

-}
