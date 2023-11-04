module Tile 
( Tile
, Tile.length
, greatest
, generate
, element
, region
, translate
, tessellate
, show
) where

import Region

data Tile a = Tile 
  { region :: Region 
  , elements :: [[Maybe a]]
  }

length :: Tile a -> Int
length tile = sum (map Prelude.length (elements tile))

generate :: Region -> (Int -> Int -> a) -> Tile a
generate region generator =
  Tile { region = region, elements = elements }
  where
    orX = fst (location region)
    orY = snd (location region)
    szX = fst (size region)
    szY = snd (size region)
    gen x y = Just (generator (orX + x) (orY + y))
    elements = map (\y -> map (\x -> gen x y) [0..szX - 1]) [0..szY - 1]

element :: Tile a -> (Int, Int) -> Maybe a
element tile (x, y) = elements tile !! y !! x

translate :: Region -> Tile a -> Tile a
translate destRegion sourceTile =
  Tile { region = destRegion, elements = elements }
  where
    (dstOrX, dstOrY) = location destRegion
    (dstSzX, dstSzY) = size destRegion
    srcRegion = region sourceTile
    (srcOrX, srcOrY) = location srcRegion
    (srcSzX, srcSzY) = size srcRegion
    isLegal (x, y) = x >= 0 && y >= 0 && x < srcSzX && y < srcSzY
    original (x, y) = (x + dstOrX - srcOrX, y + dstOrY - srcOrY)
    copy x y = if isLegal coord' 
               then element sourceTile coord' 
               else Nothing 
               where coord' = original (x, y)
    elements = map (\y -> map (\x -> copy x y) [0..dstSzX - 1]) [0..dstSzY - 1]

select :: Maybe a -> Maybe a -> Maybe a
select aElement bElement =
  case (aElement, bElement) of
    (Just aValue, Nothing)   -> aElement
    otherwise                -> bElement

tessellation :: Tile a -> Tile a -> Tile a
tessellation i j =
  Tile { region = enc, elements = elements }
  where
    enc = Region.enclosing (region i) (region j)
    iTr = translate enc i
    jTr = translate enc j
    szX = fst (size enc)
    szY = snd (size enc)
    sel x y = select (element iTr c) (element jTr c) where c = (x, y)
    elements = map (\y -> map (\x -> sel x y) [0..szX - 1]) [0..szY - 1]

tessellate :: [Tile a] -> Tile a
tessellate (first : rest) = helper first rest
  where
    helper :: Tile a -> [Tile a] -> Tile a
    helper accum [] = accum
    helper accum (next : rest) = helper (tessellation accum next) rest

instance Show a => Show (Tile a) where
  show tile = showTile tile

showTile :: Show a => Tile a -> String
showTile tile =
  show (location (region tile)) ++ " " ++ show (size (region tile)) ++ "\n"
  ++ concat (map showLine [0..(snd (size (region tile))) - 1])
  where
    padded s = s ++ replicate (4 - Prelude.length s) ' '
    showElement e = case e of
      Just value -> padded (Prelude.show value)
      Nothing    -> padded ("-")
    showLine i = concat (map showElement (Tile.elements tile !! i)) ++ "\n"

greatest :: Ord a => Tile a -> a
greatest tile = 
  case concat (elements tile) of
    [] -> undefined
    all -> maximum [element | Just element <- all]