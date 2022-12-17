module Box 
( isInterior
) where

import Data.List

import Algorithms
import Configuration
import Region

checkCorners :: Algorithm -> Configuration -> Region -> Bool
checkCorners algo config region =
  if algo config ulx uly == 0 then False
  else if algo config ulx lry == 0 then False
  else if algo config lrx uly == 0 then False
  else if algo config lrx lry == 0 then False
  else True
  where
    ulx = fst (location region)
    uly = snd (location region)
    lrx = ulx + fst (size region)
    lry = uly + snd (size region)

checkEdges :: Algorithm -> Configuration -> Region -> Bool
checkEdges algo config region =
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
    top = map (\x -> crd x 0) [0..szX - 1]
    bot = map (\x -> crd x (szY - 1)) [0..szX - 1]
    lef = map (crd 0) [1..szY - 1]
    rig = map (crd (szX - 1)) [1..szY - 1]
    hasExterior crds = find (\(x, y) -> algo config x y == 0) crds /= Nothing

isInterior :: Algorithm -> Configuration -> Region -> Bool
isInterior algo config region =
  if checkCorners algo config region == False then False
  else checkEdges algo config region
    