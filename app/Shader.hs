module Shader
( shade
) where

import ShaderAlgos
import Tile
import Region

shader :: ShaderAlgo -> Int -> Tile Int -> Int -> Int -> (Int, Int, Int)
shader algo greatest input ix iy =
  case elem of
    Just 0 -> (0,0,0)
    Just n -> algo greatest n 
    otherwise -> (0, 255, 0)
  where
    elem = element input (ix, iy)

shade :: ShaderAlgo -> Int -> Tile Int -> Tile (Int, Int, Int)
shade algo greatest input = 
  Tile.generate (Tile.region input) (shader algo greatest input)
    