module Algorithms 
( mandelbrot
, fill
, boxedFill
, boxedMandelbrot
, Algorithm
) where

import Data.Complex

import Configuration

type Algorithm = Configuration -> Int -> Int -> Int

mandelbrot :: Algorithm
mandelbrot config ix iy =
  mandelbrotEscapeIts (maxIterations config) point 0 (0 :+ 0)
  where
    stride = Configuration.stride config
    offset = (fromIntegral ix) * stride :+ (fromIntegral iy) * stride
    point = (Configuration.origin config) + offset

mandelbrotEscapeIts :: Int -> Complex Double -> Int -> Complex Double -> Int
mandelbrotEscapeIts maxIts c n z =
  if n >= maxIts then 0 -- did not escape within maxits iterations.
  else if outsideBounds then n -- escaped after n iterations.
  else mandelbrotEscapeIts maxIts c (n + 1) (z * z + c) -- still orbiting.
  where
    sq a = a * a
    outsideBounds = sq (realPart z) + sq (imagPart z) > 4.0



fill :: Algorithm
fill config ix iy = 0

boxedFill :: Int -> Int -> Algorithm
boxedFill bx by config ix iy = if ix == bx || iy == by then 255 else 0

boxedMandelbrot :: Int -> Int -> Algorithm
boxedMandelbrot bx by config ix iy = 
  if ix == bx || iy == by 
  then 255
  else mandelbrot config ix iy
  