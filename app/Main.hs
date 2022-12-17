module Main where

import Data.Complex
import System.Environment

import Algorithms
import Configuration
import Plot
import Region
import Shader
import Tile

parseArgs :: [String] -> Configuration
parseArgs args = Configuration
  { origin = origin
  , plotSize = plotSize
  , stride = stride
  , imageSize = imageSize
  , maxIterations = maxIterations }
  where
    doubs = map (\arg -> read arg :: Double) args
    origin = (doubs !! 0) :+ (doubs !! 1)
    plotSize = (doubs !! 2) :+ (doubs !! 3)
    maxIterations = truncate (doubs !! 4)
    imageWidth = doubs !! 5
    stride = (realPart plotSize) / imageWidth
    imageHeight = (imagPart plotSize) / stride
    imageSize = (truncate imageWidth, truncate imageHeight)

render :: Tile (Int, Int, Int) -> String
render tile =
  "P3\n" ++ show sx ++ " " ++ show sy ++ "\n255\n" ++ pixels
  where
    (sx, sy) = size (Tile.region tile)
    rgb (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
    pixel x y = case Tile.element tile (x, y) of
      Just px -> rgb px
      otherwise -> "255 255 255\n"
    pixels = concat (map (\y -> concat (map (\x -> pixel x y) [0..sx-1])) [0..sy-1])

main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args
  let plt = Plot.plot Algorithms.mandelbrot config
  let shd = Shader.shade plt
  putStrLn (render shd)
