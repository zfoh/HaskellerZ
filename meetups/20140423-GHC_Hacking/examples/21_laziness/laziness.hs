main :: IO ()
main = task () >> task ()

task :: () -> IO ()
task () = printvalues [1..1000000 :: Int]

printvalues :: [Int] -> IO ()
printvalues (x:xs) = print x >> printvalues xs
printvalues [] = return ()
