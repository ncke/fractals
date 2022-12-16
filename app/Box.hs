module Box 
( Box
, generate
) where

import Region

data Box a = Box
  { region :: Region
  , elements :: [a]
  }

generate :: Region -> (Int -> Int -> a) -> Box a
generate region generator =
  Box { region = region, elements = elements }
  where
    orX = fst (location region)
    orY = snd (location region)
    szX = fst (size region)
    szY = snd (size region)
    gen x y = generator (orX + x) (orY + y)
    top = map (\x -> gen x 0) [0..szX - 1]
    bot = map (\x -> gen x (szY - 1)) [0..szX - 1]
    lef = map (gen 0) [1..szY - 1]
    rig = map (gen (szX - 1)) [1..szY - 1]
    elements = concat [top, lef, rig, bot]
    