{-# LANGUAGE DuplicateRecordFields #-}

data A = A {a :: Int, b :: Double} deriving Show
data B = B {a :: String, c :: Char} deriving Show

mkA :: A
mkA = A {a = 23, b = 42}

upA :: A -> A
upA x = x { a = 12 }

sel :: A -> Int
sel = a

main :: IO ()
main = print $ sel $ upA $ mkA
