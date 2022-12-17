module Plot 
( plot
) where

--import Data.Complex ( Complex((:+)), imagPart, realPart )

import Algorithms
import Configuration
import Region
import Tile

plot :: Algorithm -> Configuration -> Tile Int
plot algo config =
  Tile.generate global (algo config)
  where
    global = Region { location = (0,0), size = imageSize config }