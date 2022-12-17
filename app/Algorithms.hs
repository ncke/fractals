module Algorithms 
( mandelbrot
, Algorithm
) where

import Data.Complex

import Configuration

type Algorithm = Configuration -> Int -> Int -> Int

mandelbrot :: Algorithm
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
