import Prelude hiding (foldr)

data D = D { get :: Int } deriving Show

plus :: D -> D -> D
plus x y = D (get x + get y)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (a:as) = foldr f (f a b) as

main = print $ foldr plus (D 0) $ map D [1..10000000]
