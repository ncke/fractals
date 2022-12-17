module Configuration
( Configuration(..)
, mainBulb
) where

import Data.Complex

data Configuration = Configuration
  { origin :: Complex Double
  , plotSize :: Complex Double
  , stride :: Double
  , imageSize :: (Int, Int)
  , maxIterations :: Int
  }

mainBulb :: Configuration
mainBulb = Configuration 
  { origin = (-2.0) :+ (-1.25)
  , plotSize = 2.5 :+ 2.5 
  , stride = 0.125 -- 20 pts
  , imageSize = (20, 20)
  , maxIterations = 100
  }