module Box 
( isInterior
) where

import Data.List

import Algorithms
import Configuration
import Region

areCornersInside :: Algorithm -> Configuration -> Region -> Bool
areCornersInside algo config region =
  if algo config ulx uly > 0 then False
  else if algo config ulx lry > 0 then False
  else if algo config lrx uly > 0 then False
  else if algo config lrx lry > 0 then False
  else True
  where
    ulx = fst (location region)
    uly = snd (location region)
    lrx = ulx + fst (size region) - 1
    lry = uly + snd (size region) - 1

areEdgesInside :: Algorithm -> Configuration -> Region -> Bool
areEdgesInside algo config region =
  if hasExterior top then False
  else if hasExterior bot then False
  else if hasExterior lef then False
  else if hasExterior rig then False
  else True
  where
    orX = fst (location region)
    orY = snd (location region)
    szX = fst (size region)
    szY = snd (size region)
    crd x y = (orX + x, orY + y)
    top = map (\x -> crd x 0) [1..szX - 2]
    bot = map (\x -> crd x (szY - 1)) [1..szX - 2]
    lef = map (crd 0) [1..szY - 2]
    rig = map (crd (szX - 1)) [1..szY - 2]
    hasExterior crds = find (\(x, y) -> algo config x y > 0) crds /= Nothing

isInterior :: Algorithm -> Configuration -> Region -> Bool
isInterior algo config region = 
  if areCornersInside algo config region
  then areEdgesInside algo config region
  else False
    