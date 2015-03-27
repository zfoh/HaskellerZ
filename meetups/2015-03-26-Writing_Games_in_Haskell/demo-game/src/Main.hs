module Main (main) where

import Graphics.Gloss

main :: IO ()
main = play window white 60 () (const $ Circle 30) (const id) (const id)
  where
    window = InWindow "Demo Game" (1200,800) (0,0)
