module Shader
( shade
) where

import Tile
import Region

shader :: Tile Int -> Int -> Int -> (Int, Int, Int)
shader input ix iy =
  case elem of
    Just 0 -> (0,0,0)
    Just n -> (min 255 (n * 9), 0, 0)
    otherwise -> (255, 255, 255)
  where
    elem = element input (ix, iy)

shade :: Tile Int -> Tile (Int, Int, Int)
shade input =
  Tile.generate (Tile.region input) (shader input)
  where
    