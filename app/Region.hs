module Region
( Region(..)
, enclosing
, quadrants
) where

data Region = Region
  { location :: (Int, Int)
  , size :: (Int, Int) 
  } deriving (Show)
  
xMin :: Region -> Int
xMin region = fst (location region)

yMin :: Region -> Int
yMin region = snd (location region)

xMax :: Region -> Int
xMax region = xMin region + fst (size region)

yMax :: Region -> Int
yMax region = yMin region + snd (size region)

enclosing :: Region -> Region -> Region
enclosing a b = Region { location = locn, size = sz }
  where
    locn = (min (xMin a) (xMin b), min (yMin a) (yMin b))
    farside = (max (xMax a) (xMax b), max (yMax a) (yMax b))
    sz = (fst farside - fst locn, snd farside - snd locn)

quadrants :: Region -> [Region]
quadrants a =
  [ Region { location = (lx, ly), size = (x1, y1) }
  , Region { location = (lx, ly + y1), size = (x1, y2) }
  , Region { location = (lx + x1, ly), size = (x2, y1) }
  , Region { location = (lx + x1, ly + y1), size = (x2, y2) } ]
  where
    (lx, ly) = (fst (location a), snd (location a ))
    (sx, sy) = (fst (size a), snd (size a))
    x1 = sx `div` 2
    x2 = sx - x1
    y1 = sy `div` 2
    y2 = sy - y1
