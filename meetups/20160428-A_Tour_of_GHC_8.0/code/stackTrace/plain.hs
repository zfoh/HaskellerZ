import Prelude hiding (head)

head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

f :: [Int] -> Int
f xs = case head xs of
  0 -> 0
  _ -> 1

g :: [Int] -> Int
g [] = 0
g (x:xs) = x + g xs + f xs

main = print $ g [1,2,3]
