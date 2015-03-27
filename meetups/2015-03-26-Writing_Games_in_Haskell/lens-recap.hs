{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Char

--------------------------------------------------------------------------------

data MyData = MD { _foo :: Int
                 , _bar :: Int
                 } deriving (Show)

foo :: Lens' MyData Int
foo = lens (\myData -> _foo myData)
           (\oldData newFoo -> oldData { _foo = newFoo })

d1 :: MyData
d1 = MD 5 10


-- > view foo d1
-- > set foo 8 d1
-- > d1 ^. foo
-- > :t foo .~ 8
-- > foo .~ 8 $ d1
-- > d1 & foo .~ 8





--------------------------------------------------------------------------------
-- Composability

x1 = ("Hello", (MD 5 10, False))

-- > x1 ^. _2._1.foo
-- > x1 & _2._1.foo .~ 8
-- > x1 & _2._1.foo +~ 8
--
-- > _foo . fst . snd $ x1
-- > view (_2._1.foo) x1





--------------------------------------------------------------------------------
-- Traversals

l1 = [ ("one", MD 10 100)
     , ("two", MD 20 200)
     , ("three", MD 30 300)
     ]

l2 :: [Int]
l2 = [ 0, 10, 20, 30 ]

-- > l1 ^.. traversed._2.foo
-- > l1 & traversed._2.foo +~ 1
-- > l1 & traversed._1.traversed .~ 'x'
-- > l1 & traversed._1.traversed %~ toUpper
-- > l2 & traversed %@~ (\i x -> i + x)
-- > l2 & traversed %@~ (+)
-- > l2 & traversed %@~ (\i x -> 3*i + x)





--------------------------------------------------------------------------------
-- Automatic generation

data MyData2 a = MD2 { _baz  :: Int
                     , _quux :: a
                     } deriving (Show)

makeLenses ''MyData2

d2 :: MyData2 Bool
d2 = MD2 10 False

-- > :t baz
baz' :: Lens' (MyData2 a) Int
baz' = baz
-- > :t baz'

-- > :t quux
quux' :: Lens (MyData2 a) (MyData2 b) a b
quux' = quux
-- > :t quux'

-- > d2 & quux .~ "Hello"
-- > :t d2
-- > :t d2 & quux .~ "Hello"
