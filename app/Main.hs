module Main where

import Data.Complex
import System.Environment
import Configuration
import Region
import Tile

parseArgs :: [String] -> Configuration
parseArgs args = Configuration
  { origin = origin
  , plotSize = plotSize
  , stride = stride
  , imageSize = imageSize
  , maxIterations = 100 }
  where
    doubs = map (\arg -> read arg :: Double) args
    origin = (doubs !! 0) :+ (doubs !! 1)
    plotSize = (doubs !! 2) :+ (doubs !! 3)
    imageWidth = doubs !! 4
    stride = (realPart plotSize) / imageWidth
    imageHeight = (imagPart plotSize) / stride
    imageSize = (truncate imageWidth, truncate imageHeight)

globalRegion :: Configuration -> Region
globalRegion config = Region { location = (0, 0), size = imageSize config }

main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args

  putStrLn "Hello, Haskell!"
