module Region
( Region(..)
, enclosing
) where

data Region = Region
  { location :: (Int, Int)
  , size :: (Int, Int) 
  }
  
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
