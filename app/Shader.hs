module Shader
( shade
) where

import ShaderAlgos
import Tile
import Region

shader :: ShaderAlgo -> Tile Int -> Int -> Int -> (Int, Int, Int)
shader algo input ix iy =
  case elem of
    Just 0 -> (0,0,0)
    Just n -> algo n 
    otherwise -> (0, 255, 0)
  where
    elem = element input (ix, iy)

shade :: ShaderAlgo -> Tile Int -> Tile (Int, Int, Int)
shade algo input = Tile.generate (Tile.region input) (shader algo input)
    