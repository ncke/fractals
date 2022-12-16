module Tile 
( Tile
, generate
, tessellate
) where

import Region

data Tile a = Tile 
  { region :: Region 
  , elements :: [[Maybe a]]
  }

generate :: Region -> (Int -> Int -> a) -> Tile a
generate region generator =
  Tile { region = region, elements = elements }
  where
    orX = fst (location region)
    orY = snd (location region)
    szX = fst (size region)
    szY = snd (size region)
    gen x y = Just (generator (orX + x) (orY + y))
    elements = map (\x -> map (gen x) [0..szY - 1]) [0..szX - 1]

element :: Tile a -> (Int, Int) -> Maybe a
element tile coord = elements tile !! fst coord !! snd coord

translate :: Region -> Tile a -> Tile a
translate dest input =
  Tile { region = dest, elements = elements }
  where
    orX = fst (location dest)
    orY = snd (location dest)
    szX = fst (size dest)
    szY = snd (size dest)
    inputRegion = region input
    inputLocn = location inputRegion
    inputSize = size inputRegion
    isLegal (x, y) = x >= 0 && y >= 0 && x < fst inputSize && y < snd inputSize
    original (x, y) = (x + orX - fst (location (region input)), y + orY - snd (location (region input)))
    copy x y = if isLegal coord' then element input coord' else Nothing where coord' = original (x, y)
    elements = map (\x -> map (copy x) [0..szY - 1]) [0..szX - 1]

select :: Maybe a -> Maybe a -> Maybe a
select aElement bElement =
  case (aElement, bElement) of
    (Just aValue, Nothing)   -> aElement
    otherwise                -> bElement

tessellation :: Tile a -> Tile a -> Tile a
tessellation i j =
  Tile { region = reg, elements = elements }
  where
    reg = bounding (region i) (region j)
    iTranslated = translate reg i
    jTranslated = translate reg j
    szX = fst (size reg)
    szY = snd (size reg)
    select' x y = select (element iTranslated coord') (element jTranslated coord') where coord' = (x, y)
    elements = map (\x -> map (select' x) [0..szY - 1]) [0..szX - 1]

tessellate :: [Tile a] -> Maybe (Tile a)
tessellate [] = Nothing
tessellate (first : rest) = Just (helper first rest)
  where
    helper :: Tile a -> [Tile a] -> Tile a
    helper accum [] = accum
    helper accum (next : rest) = helper (tessellation accum next) rest

