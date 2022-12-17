module Plot 
( plot
) where

import Data.Complex ( Complex((:+)), imagPart, realPart )

import Configuration
import Region
import Tile

mandelbrot :: Configuration -> Int -> Int -> Int
mandelbrot config ix iy =
  recur 0 (0 :+ 0)
  where
    maxits = maxIterations config
    offset = (fromIntegral ix) * stride config :+ (fromIntegral iy) * stride config
    start = origin config + offset
    recur n posn = if n >= maxits then 0
                   else if outsideBounds then n 
                   else recur (n + 1) newposn
      where
        sq a = a * a
        outsideBounds = sq (realPart posn) + sq (imagPart posn) > 4.0
        newposn = sq posn + start

plot :: Configuration -> Tile Int
plot config =
  Tile.generate global (mandelbrot config)
  where
    global = Region { location = (0,0), size = imageSize config }