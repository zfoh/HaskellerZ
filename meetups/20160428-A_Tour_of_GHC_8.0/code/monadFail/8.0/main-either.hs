import Compute

main :: IO ()
main = print $ (f 23 :: Either Char Int)
