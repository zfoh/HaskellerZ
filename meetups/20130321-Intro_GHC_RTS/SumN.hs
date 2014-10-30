module SumN_Example where


sumN :: Int -> Int
sumN 0 = 0
sumN n = n + sumN (n - 1)

sumNAcc :: Int -> Int
sumNAcc =
    go 0
  where
    go acc 0 = acc
    go acc n = go (n + acc) (n -1)

-- test :: Int
-- test = sumN 0xDeadBeef
