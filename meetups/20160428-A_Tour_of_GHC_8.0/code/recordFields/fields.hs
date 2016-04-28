{-# LANGUAGE OverloadedRecordFields #-}

data Person  = Person  { personId :: Int, name :: String }
data Address = Address { personId :: Int, address :: String }

getId :: r { personId :: Int } => r -> Int
getId x = #personId x

main = do
  print $ getId $ Person 23 "foo"
  print $ getId $ Address 42 "bar"
