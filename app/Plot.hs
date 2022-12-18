module Plot 
( plot
) where

--import Data.Complex ( Complex((:+)), imagPart, realPart )

import Algorithms
import Box
import Configuration
import Region
import Tile

quadPlot :: Algorithm -> Configuration -> Region -> Tile Int
quadPlot algo config region =
  if Box.isInterior algo config region
  then Tile.generate region (Algorithms.boxedFill bx by config)
  else if fst (size region) < 20 then Tile.generate region (Algorithms.boxedMandelbrot bx by config)
  else tessellate (map (quadPlot algo config) (quadrants region))
  where
    (bx, by) = (fst (location region), snd (location region))

--quadPlot :: Algorithm -> Configuration -> Region -> Tile Int
--quadPlot algo config region =
--  if Box.isInterior algo config region
--  then Tile.generate region (Algorithms.fill config)
--  else if fst (size region) < 20 then Tile.generate region (algo config)
--  else tessellate (map (quadPlot algo config) (quadrants region))

plot :: Algorithm -> Configuration -> Tile Int
plot algo config =
  if Configuration.isConnected config 
  then quadPlot algo config global
  else Tile.generate global (algo config)
  where
    global = Region { location = (0,0), size = imageSize config }