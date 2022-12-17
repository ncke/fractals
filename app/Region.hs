module Region
( Region(..)
, enclosing
) where

data Region = Region
  { location :: (Int, Int)
  , size :: (Int, Int) 
  }

type Extent = ((Int, Int), (Int, Int))

extent :: Region -> Extent
extent region = (nearside, farside)
  where 
    nearside = location region
    size' = size region
    farside = (fst nearside + fst size', snd nearside + snd size')
  
xMin :: Extent -> Int
xMin extent = fst (fst extent)

yMin :: Extent -> Int
yMin extent = snd (fst extent)

xMax :: Extent -> Int
xMax extent = fst (snd extent)

yMax :: Extent -> Int
yMax extent = snd (snd extent)

merge :: Extent -> Extent -> Extent
merge a b = (nearside, farside)
  where
    nearside = (min (xMin a) (xMin b), min (yMin a) (yMin b))
    farside = (max (xMax a) (xMax b), max (yMax a) (yMax b))

extentLocation :: Extent -> (Int, Int)
extentLocation extent = (xMin extent, yMin extent)

extentSize :: Extent -> (Int, Int)
extentSize extent = (xMax extent - xMin extent, yMax extent - yMin extent)

enclosing :: Region -> Region -> Region
enclosing a b =
  Region { location = extentLocation enclosure, size = extentSize enclosure }
  where enclosure = merge (extent a) (extent b) 
