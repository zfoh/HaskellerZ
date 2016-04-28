{-# LANGUAGE ImplicitParams #-}

foo :: (?flt :: a -> Bool) => [a] -> [a]
foo = filter ?flt

bar = foo ([1,2,3] :: [Int])
  where ?flt = (==1)

main = print bar
