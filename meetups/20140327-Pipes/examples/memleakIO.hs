-- Does leak with -O0
-- Does not leak with -O

printNumbers :: IO ()
printNumbers = go 0
  where
    go :: Int -> IO ()
    go n = do print n
              go (n+1)

main :: IO ()
main = do
  printNumbers
  printNumbers
