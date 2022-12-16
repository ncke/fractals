module Configuration
( Configuration(..)
) where

import Data.Complex

data Configuration = Configuration
  { origin :: Complex Double
  , plotSize :: Complex Double
  , stride :: Double
  , imageSize :: (Int, Int)
  , maxIterations :: Int 
  }
