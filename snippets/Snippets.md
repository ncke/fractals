# Snippets

## Build and run using Cabal.

`cabal run -v0 mandelbrot -- -0.95 0.0 0.4 0.4 250 600 > op.ppm`

## Modifications to demo quad plots.

A version of `quadPlot` in `Plot.hs` that uses `boxedFll` and `boxedMandelbrot` when plotting.

```haskell
quadPlot :: Algorithm -> Configuration -> Region -> Tile Int
quadPlot algo config region =
  if Box.isInterior algo config region
  --then Tile.generate region (Algorithms.fill config)  
  then Tile.generate region (Algorithms.boxedFill bx by config)
  --else if fst (size region) < 20 then Tile.generate region (Algorithms.mandelbrot config)
  else if fst (size region) < 20 then Tile.generate region (Algorithms.boxedMandelbrot bx by config)
  else tessellate (map (quadPlot algo config) (quadrants region))
  where
    (bx, by) = (fst (location region), snd (location region))
```

And change the shader to show the box edge (255s) in colour.

```haskell
shader :: Tile Int -> Int -> Int -> (Int, Int, Int)
shader input ix iy =
  case elem of
    Just 0 -> (0,0,0)
    Just 255 -> (103, 158, 156)
    --Just n -> (min 254 (n * 9), 0, 0)
    Just n -> (min 254 (n * 9), 0, 0) 
    otherwise -> (0, 255, 0)
  where
    elem = element input (ix, iy)
```
