module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck

import Region
import Tile

-- Region.

prop_regionLocation :: (Int, Int) -> (Int, Int) -> Bool
prop_regionLocation locn size = (location reg) == locn
  where
    reg = Region { location = locn, size = size }

prop_regionSize :: (Int, Int) -> (Int, Int) -> Bool
prop_regionSize locn rsize = (size reg) == rsize
  where
    reg = Region { location = locn, size = rsize }

-- Tile.

prop_tileGenerate :: (Int, Int) -> (Int, Int) -> Property
prop_tileGenerate locn (xSize, ySize) =
  (xSize > 0 && xSize < 100 && ySize > 0 && ySize < 100) ==> Tile.length tile == (xSize * ySize)
  where
    reg = Region { location = locn, size = (xSize, ySize) }
    genr x y = x + y
    tile = Tile.generate reg genr

prop_tileElement :: (Int, Int) -> Property
prop_tileElement (x, y) =
  (x >= 0 && x < 100 && y >= 0 && y <= 100) ==> Tile.element tile (x, y) == Just (x + y)
  where
    reg = Region { location = (0,0), size = (100, 100) }
    genr x y = x + y
    tile = Tile.generate reg genr

main :: IO ()
main = do
  result <- do
    putStrLn ("region location") 
    quickCheckResult prop_regionLocation
    putStrLn ("region size")
    quickCheckResult prop_regionSize
    putStrLn ("tile generate")
    quickCheckResult prop_tileGenerate
    putStrLn ("tile element")
    quickCheckResult prop_tileElement
  
  unless (isSuccess result) exitFailure
