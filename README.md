# fractals
Messing about plotting fractals with Haskell.

![Main bulb of the Mandelbrot set](https://github.com/ncke/fractals/blob/c817a10e9ed66f838480e6a9e04c54699479f8a0/examples/mainbulb-900x900.png)

**Figure 1.** The main bulb of the Mandelbrot set.
`mandelbrot -2.0 -1.25 2.5 2.5 250 900 > op.ppm`

## Command line reference.

`mandelbrot XORIG YORIG WIDTH HEIGHT MAXITS PIXELS` streams P3 format image data to the standard output.

`XORIG`  the x-coordinate of the lower-left corner of the region to be plotted in the complex plane.

`YORIG`  the y-coordinate of the lower-left corner of the region.

`WIDTH`  the width of the plot in the complex plane.

`HEIGHT` the height of the plot in the complex plane.

`MAXITS` the maximum number of iterations before assuming set membership.

`PIXELS` the width of the result image in pixels (height is determined to match the aspect ratio of the region).

## What are fractals?
Fractals are a kind of shape. Unlike ordinary shapes, such as squares and circles, a fractal has the property of self-similarity. This means that no matter how deeply you zoom in on some piece of the fractal, the original motif continues to emerge at an ever smaller scale.

One of the reasons why fractals are mathematically interesting is because self-similarity can also be observed in many natural shapes. For example, a satellite view of a coastline will have bays and promontories. But zooming-in reveals a more jagged edge with smaller bays and inlets. This repetition of coastal features at different scales is an example of self-similarity.

The intricacy in the coastline is created by the processes of coastal erosion — the interaction of the waves and wind at the shoreline. The overall shape arises from macroscopic features such as rock types, tidal flows, and prevailing winds. But elaborate details emerge because previous weathering influences future erosion — perhaps channelling water towards or away from a particular section. Even small changes inside these feedback loops will compound across geological timescales to create significant differences.

More dynamic systems, such as the weather, are so sensitive that it is difficult to forecast with accuracy at more than a few days or weeks in advance.  This sensitivity is a part of chaos theory known as the Butterfly Effect, thanks to [Edward Lorenz](https://en.wikipedia.org/wiki/Edward_Norton_Lorenz))’s suggestion that the tiny perturbations caused by a butterfly’s wing could (at least in theory) influence a chain of atmospheric events ending in a hurricane several weeks later.

Fractals and chaos theory have changed the way that we think about engineered systems. We are no longer surprised when complex behaviour emerges from interacting components, particularly when a feedback loop is involved. More importantly, it is also fun to generate fractals for ourselves and look at them using our computers.

There’s a lot more information about chaos theory and fractals in James Gleick’s book ‘Chaos: Making a New Science’, which is a classic [1].

## The Mandelbrot set.
We have seen that fractals are shapes that are self-similar at many scales, a property sometimes seen in nature. The mathematician [Benoit B. Mandelbrot](https://en.wikipedia.org/wiki/Benoit_Mandelbrot) described many other naturally occurring fractals [2]. He also gave his name to one of the most well-known mathematical fractals: the Mandelbrot set.

The Mandelbrot set is a distinctive collection of points in the complex plane. The complex plane exists because of complex numbers, and complex numbers exist to answer the question of how to find the square root of negative one.

For our purposes, the most important thing to know is that a complex number consists of two parts: a real part and an imaginary part. The idea of a number being imaginary is to do with that square root, but we won’t go into that now. We just  need to know how to write a complex number like this:

  2.5 + 3.2i

The real part of the number is 2.5, and the imaginary part is 3.2. With this in mind we can now make two simplifying observations.

Firstly, the complex plane is two dimensional. Each complex number sits at a position determined by the real part, which is normally measured along the x-axis, and the imaginary part, which is measured along the y-axis. Computer screens are organised in the same way, which is ideal because we can easily map regions of the complex plane into images.

The second observation has to do with the fact that the complex plane extends infinitely in all directions; the real and imaginary parts can be as large as you like in both the positive and negative sense. Computer screens are not infinite, despite what you might occasionally see on [r/battlestations](https://www.reddit.com/r/battlestations/comments/11c0so7), so this could be inconvenient. The good news, however, is that no point in the Mandelbrot is more than two units from the origin at 0 + 0i. So that’s okay then.

Figure 1 above shows the Mandelbrot set sitting in the complex plane. Points that are members of the set are conventionally coloured black; and the membership test is described below. In this figure the points that are outside the set are shaded red, later on we discuss how to shade these points to produce more colourful plots.

## How it works.

👉 **Disclaimer.** I’m not an Haskell expert, I’m a beginner. I’m teaching myself Haskell because I think that functional languages are ace and because I think that learning to express yourself in different programming languages helps you to write better code overall (cf, the (Sapir-Whorf Hypothesis)[https://plato.stanford.edu/entries/linguistics/whorfianism.html]).

The `Main` function implements a four-step process, shown in Figure 2 below.

<img src="https://github.com/ncke/fractals/blob/bb3a727447ac90038eb9ac2508f24b194f7baed5/resources/figure-2.png" width=600>

**Figure 2.** The four-step process of producing a fractal image.

This is how that looks in code:

```haskell
main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args
  let plt = Plot.plot Algorithms.mandelbrot config
  let shd = Shader.shade plt
  putStrLn (render shd)
```

First of all we interpret the command line arguments to set up a `Configuration`. The configuration contains all of the details needed to control the next steps, for example, the coordinates for the region of the complex plane that we want to examine.

Secondly, we perform the plot itself. At this stage we aren’t thinking about an image format or colours. Instead, we concentrate on the mathematics of the fractal itself at each point in the region of interest.

We have a polymorphic data type called a `Tile a` that stores all of the values in a rectangular area. And, in fact, we have a data type called `Region` that we use to specify exactly where that rectangular area is and its size. Without going into too much detail yet, we now have a populated  `Tile Int`.

In the third stage we shade each of those points. We go back over the tile and generate RGB values which now gives us a `Tile (Int, Int, Int)`.

The fourth and final step is to render the RGB tile into an image that we can view. For simplicity, and to avoid external dependencies, we use the P3 (.ppm) format. P3 is text-based and uncompressed. This is a little hungry for disk space, but this is not a project about image compression. On a Mac, the Preview app will handle P3 images or we can use a utility like ToyViewer [3].

## Plotting.
The most interesting parts of the fractal-producing process are plotting and shading. Let’s discuss plotting first; this is organised in the `Plot` module, but most of the work happens in the `Tile` and `Algorithm` modules.

#### More about tiles.

We already know that we are going to generate a fully populated `Tile Int`.  The tile data type isn’t just a container for Ints. Instead it knows that it is a tile — some bounded rectangular zone in the plane. So it has a `region` property to describe the location, and an `elements` property to hold the actual values. 

When you think about it, the properties for the region and the elements in a tile are inextricably linked. We can’t just take the elements from one tile and plonk them together with the region from another tile. At least, if we did do that then the outcome would not be a valid reflection of the reality that we are trying to model. To prevent this, `Tile` is implemented as an abstract data type. We use encapsulation to hide away implementation details as well as to maintain the conceptual purity of a tile. For example, we don’t expose how our storage of the elements is actually arranged. It’s a list of lists, but another programmer doesn’t need to know that, they can just use the `element` function to query for a value at a particular x-y location.

More significantly, we abstract away the process for creating a `Tile` so that the relationship between the region and its elements is enforced. Instead of exposing the initialiser, we provide a `generate` function that must be used to create tiles. The type signature looks like this:

  `generate :: Region -> (Int -> Int -> a) -> Tile a`

We pass in a region, and then we pass in a ‘strategy’ for generating the elements of that region; and, in return, the `generate` function will use the strategy to give us a fully populated tile. The tile takes responsibility for running the strategy for each and every location in the region, so it's not possible to mix up the elements for a different region.

The strategy itself has the signature `(Int -> Int -> a)`, this type has actually been given the alias `Algorithm` but we don’t use that here to avoid a dependency cycle. The two integers are an x-y position inside the tile, it’s the job of the strategy implementation to take that position and return the corresponding value of type `a`. One thing to note here is that the tile doesn’t know about the coordinate system that complex numbers use (it's more general than that). As far as the tile is concerned, every element occurs at a 2-D integer point, like (5, 14) for example. It’s up to the strategy algorithm to map this to whatever coordinate system it wants to use, we will see how that is done next.

#### A strategy for the Mandelbrot set.

So how does this work for the Mandelbrot set in the complex plane. The `Tile` is going to call our plotting strategy with some x-y Ints, like (5, 14), how do we handle that?

The first thing to do is convert the integer coordinates that we have been given by the tile into a position in the complex plane. We have the `Configuration` instance to guide us. Figure 3 below shows the general idea. We know, from the configuration, the complex coordinate for the lower right hand corner, in this example it is -2.0 - 1.25i and the size of the region is 2.0 + 2.0i; and we are also given the overall image size in pixels. So for any given x-y pair we can use linear interpolation to work out the corresponding position in the complex plane.

![Linear interpolation to the complex plane](https://github.com/ncke/fractals/blob/ef970f3ecec8fe230ed4d77ca9b98b8f278729eb/resources/figure-3.png)

**Figure 3.** (a) How the tile sees it, a 20x20 grid of points is shown with (5, 14) highlighted. (b) How the Mandelbrot strategy uses the configuration data structure to map onto the complex plane. For the real part, 5 * 2.5 / 20 = 0.625, and for the imaginary part 14 * 2.5i / 20 = 1.75i. Adding this offset to the origin at the lower left gives us -1.375 + 0.5i.

Now what? We’ve got our complex number, how do we know whether it is inside the Mandelbrot set or not? The equation is shown below at Equation 1. Given the intricacy of the Mandelbrot set, the suprising thing about Equation 1 is how simple it is. That is all it takes to set up a feedback loop that will create an enormously complex shape (infinitely complex, in fact). The concision of this chaos is one of the reasons why the Mandelbrot set is so admired.

<img src="https://github.com/ncke/fractals/blob/4d033336e348e9b7d97d5a59e4586222f0a74702/resources/equation-1.png" width=300>

**Equation 1.** The Mandelbrot set.

We iterate this equation so that z<sub>0</sub> produces z<sub>1</sub>, which produces z<sub>2</sub>, which produces z<sub>3</sub>, and so forth. The process of iteration always starts at the origin, so z<sub>0</sub> is always 0.0 + 0.0i. And c is kept constant throughout, it's the point of interest. In our example, c is -1.375 + 0.5i. We carry on iterating to see if z<sub>n</sub> is ever more than 2.0 units away from the origin. If that happens then we know for sure that the point (c) is not in the Mandelbrot set. If a point is in the set then we could carry on iterating forever, z<sub>n</sub> will always orbit the origin and will never fly off. But iterating forever would take a long time and we're impatient to see our fractal. So we set an arbitrary maximum limit to the number of iterations before we just assume that the point is in orbit and therefore in the set. In the plot for Figure 1 that limit was 250 iterations.

Remember that, for plotting, we want to get a `Tile Int` data type. Where does the Int come from? If we hit the maximum limit for the number iterations then we populate that the location with a 0 to say that the point is inside the Mandelbrot set. Otherwise we return the number of iterations that were achieved before the z<sub>n</sub> flew out of orbit beyond 2.0. We could do something a bit more fancy than just use 0 to denote a point in the set, but this is a commonly used representation and it keeps things simple.
  
Figure 4 shows how this all works out for our example point at -1.375 + 0.5i. Note that complex numbers have their own special way of performing multiplication, we use `Data.Complex` to handle this for us.
  
<img src="https://github.com/ncke/fractals/blob/4d033336e348e9b7d97d5a59e4586222f0a74702/resources/figure-4.png" width=600>

**Figure 4.** Tracking successive iterations for the point c = -1.375 + 0.5i until the 2.0-unit radius circle is escaped at z<sub>3</sub>.

In figure 4 we can see that the point escapes after only three iterations, so we can say that this point is not a member of the Mandelbrot set. The algorithm returns `n = 3` in this case. Speaking loosely, `n` is a kind of measure of how 'close' the point is to the set. It provides the basis for shading the chart, as we'll see later.
  
So here's our algorithm in full:
  
```haskell
type Algorithm = Configuration -> Int -> Int -> Int

mandelbrot :: Algorithm
mandelbrot config ix iy =
  mandelbrotEscapeIts (maxIterations config) point 0 (0 :+ 0)
  where
    stride = Configuration.stride config
    offset = (fromIntegral ix) * stride :+ (fromIntegral iy) * stride
    point = (Configuration.origin config) + offset

mandelbrotEscapeIts :: Int -> Complex Double -> Int -> Complex Double -> Int
mandelbrotEscapeIts maxIts c n z =
  if n >= maxIts then 0 -- did not escape within maxits iterations.
  else if outsideBounds then n -- escaped after n iterations.
  else mandelbrotEscapeIts maxIts c (n + 1) (z * z + c) -- still orbiting.
  where
    sq a = a * a
    outsideBounds = sq (realPart z) + sq (imagPart z) > 4.0
```

That last line determines whether the point has left orbit, it has a little trick. We use the Pythagorean formula to calculate displacement from the origin, but square roots can be expensive to compute. We leave the number as a square and compare with 4.0 (which is 2.0<super>2</super>) so it's the same test. This optimisation is useful. Plotting an 800 x 800 pixel fractal involves testing 640,000 points in the complex plane. Suppose we set the iteration maximum at 250, then, in a worst-case scenario, there will be 160 million iterations. In Big O-notation, we would say that the algorithm for plotting an n x n pixel fractal is O(n<sup>2</sup>) which is pretty fierce considering that we want nice big images with lots of pixels.

Actually though, there's a much better trick that we can also employ to speed things along. Let's talk about that next.

## Improving plotting performance.

Mathematicians assure us that the Mandelbrot set is connected. Adrien Douady and John Hubbard proved this using complex analysis in 1982 [4], also there is also a 2001 topological proof by Jeremy Khan [5]. Indeed, one of the topological implications of connectedness is that the Mandelbrot set has no holes. If you look at Figure 1, then you will see the black islands of the set surrounded by a sea of colour; but you will never find a sea of colour inside a black island. A more subtle way of putting that is that you can draw a box anywhere on the plot, and if all the points on the edge of box are in the Mandelbrot set (black), then all the points inside the box are also in the Mandelbrot set.

We can use this topological property to our advantage. Plotting points that are inside the Mandelbrot set requires maximum computational expense because each runs fully to the maximum iteration limit. But if we draw a box and find that all points on the edge are inside the set then there’s no need to compute the interior points — we can immediately mark them as set members. If, however, we find a single non-member point on the edge then all bets are off. We must then either explicitly compute all of the interior points -- or we could subdivide the box into smaller boxes and try again! But say that we can draw a 200 x 200 box and find that all of the edge points are in the Mandelbrot set, with a 250 iteration limit, that edge test costs us 200 thousand iterations and it allows us to avoid 10 million iterations for full computation. The test is O(n) while full computation is O(n<sup>2</sup>). In an ideal world, even the work done to compute the test points would be merged into the interior computation (although this is not currently implemented here).

To get these savings working we introduce the ‘quad plot’. When given a region to plot we first apply the box test. If the test succeeds we generate a tile using the `Algorithm.fill` strategy because we know that all interior points are in the set (`fill` let's us just black out the tile). However, if the test fails, then we divide the region into four quadrants and repeat the process for each smaller area. We could push this recursion all the way to a 1x1 box, but in reality we check to see if the region width is less than 20 pixels and if so we plot it use `Algorithm.mandelbrot` to compute all of the interior points. This is a trade-off that balances the increasing cost of recursion versus the diminishing benefit of our optimisation for smaller boxes.

Anyway, here’s the code for quad plotting:

```haskell
plot :: Algorithm -> Configuration -> Tile Int
plot algo config =
  if Configuration.isConnected config 
  then quadPlot algo config global
  else Tile.generate global (algo config)
  where global = Region { location = (0,0), size = imageSize config }

quadPlot :: Algorithm -> Configuration -> Region -> Tile Int
quadPlot algo config region =
  if Box.isInterior algo config region
  then Tile.generate region (Algorithms.fill config)
  else if fst (size region) < 20 then Tile.generate region (algo config)
  else tessellate (map (quadPlot algo config) (quadrants region))
```

This is  going to improve our  time efficiency in most cases, but what about space efficiency? Each of those quadrants is trying to populate a portion of the bigger tile; and each quadrant can itself be divided into smaller quadrants. If that recursion reaches, say, five deep then there will 1,024 (4<sup>5</sup>) quadrants. Instead of allowing each quadrant to write to its own copy of the full tile, we give each quadrant a tile that is custom-sized for its smaller region. The `Tile` module has a `tessellate` function that we can use later to merge two or more tiles together to form a larger tile that is just big enough to hold them all.

Figure 5(a) shows an individual 5x5 quadrant that has been populated for demonstration purposes. In figure 5(b) that quadrant has been tessellated with three others into a 10x10 quadrant. After the individual quadrants have been assimilated they will be subjected to the tender mercy of the garbage collector.

```
(10,10) (5,5)
1  1  1  1  1
1  1  1  1  1
1  1  1  1  1
1  1  1  1  1
1  1  1  1  1
```
**Figure 5(a).** An individual 5x5 tile.

```
Tile.tessellate [tile1, tile2, tile3, tile4]

(10,10) (10,10)
1  1  1  1  1  2  2  2  2  2
1  1  1  1  1  2  2  2  2  2
1  1  1  1  1  2  2  2  2  2
1  1  1  1  1  2  2  2  2  2
1  1  1  1  1  2  2  2  2  2
3  3  3  3  3  4  4  4  4  4
3  3  3  3  3  4  4  4  4  4
3  3  3  3  3  4  4  4  4  4
3  3  3  3  3  4  4  4  4  4
3  3  3  3  3  4  4  4  4  4
```
**Figure 5(b).** Four 5x5 tiles tessellated to form a 10x10 tile.

To demonstrate this in action, we can adapt the shader to highlight the tesselations in a full plot. Figure 6 shows the main bulb replotted to show how it is divided into quadrants and sub-quadrants. The smallest box size still predominates, and these are fully plotted. However, the presence of larger boxes indicates that significant work has been saved in the most computationally expensive regions because of the box test.

![Box test](https://github.com/ncke/fractals/blob/3650000245d73e7f4ae6eeec0b31d2034a67ddad/resources/figure-6.png)

**Figure 6.** Using the box test to improve plotting efficiency.

The box test is implemented by the `Box` module. As a further optimisation, the four corners of the box are tested first. If the corners pass, then each of the edges is tested in full. This corner-led approach usually serves to disqualify a box sooner. Having mentioned the shader, it's time to look at how we can assign colours to points outside the set.

## Shading.

So, points that are inside the Mandelbrot set are conventionally plotted as black. But how should we colourise the outside points? Well, for each outside point, we know the number of iterations that it took before the iteration exited that 2.0-unit circle never to return (see Figure 4 above if you need a refresher). We need to somehow turn that count into an RGB value for our final image.

One of the simplest ways to map iteration counts to RGB is to pick one of the colour channels (Red, say) and assign the count into that channel. Just make sure that the assigned value doesn't exceed 255 (for 8-bit colour) and we've got a monochromatic aesthetic -- and we can make that black-and-white by assigning the same count to all channels. In fact, that's how Figure 7(a) below was produced.

|(a)       |(b)       |
|----------|----------|
|![Figure 7(a)](https://github.com/ncke/fractals/blob/f05d4ea83744d03afac03a7be1742105462411c5/resources/figure-7a.png)|![Figure 7(b)](https://github.com/ncke/fractals/blob/f05d4ea83744d03afac03a7be1742105462411c5/resources/figure-7b.png)|

**Figure 7.** Shading the Mandelbrot main bulb in black and white: in image (a) intensity is determined by the raw iteration count, while in image (b) the intensity is brightened by scaling the iteration count by a factor of ten.

If Figure 7(a) looks a little dim, it's because most exterior points leave the 2.0-unit orbit in only a few iterations. These appear almost black and darkness predominates. Only at the edge of the set do points last long enough to make it to brigher whites. We can boost the signal, though. If we multiple each iteration count by a constant (such as, 10) then we brighten those edges making it easier to see the fine tracery -- see Figure 7(b). However, there is a downside: the boundary between iteration counts now becomes visible.

In any case, perhaps we can make things more colourful?

## Improving the shader.

It turns out that there are many many ways of assigning colour values to the Mandelbrot set's exterior region. But they all work in the same way at least at first glance. They all map from an iteration count to an RGB triplet, and for scaling purposes it is useful to know the greatest number of iterations encounted in the plot.

Therefore, given this uniform approach, it makes sense to abstract the colouring algorithm into a type called a `ShaderAlgo`:

```haskell
type ShaderAlgo = Int -> Int -> (Int, Int, Int)
```

A `ShaderAlgo` will take 

## Rendering an image.

Now that we have a tile of RGB-values, the next step is to turn that into something that we can see. We need to convert the data in the RGB tile into data organised as an image format. The P3 or PPM image format is a human-readable text-based format. This has several advantages: we can inspect the values to spot issues, and we can output text directly from the executable without introducing a third-party dependency to handle a complicated graphics format.

P3 image output looks like this:

```
P3
900 900
255
103 158 156
103 158 156
103 158 156
...
```

The first line is a preamble, the next is the image size in terms of width and height (900x900), and after that '255' is the maximum colour value. We're outputting 8-bit RGB-values, so 255 is the greatest value we will use for any colour component. The remainder of the file is a list of RGB-values organised row-by-row so that the first-value pertains to the top left pixel and the last value to the bottom-right pixel. As our example image is 900 x 900 pixels in size, there will be 810,000 such values each comprising three integers separated by a space. The simplicity of P3 comes at the cost of disk space, the 900 x 900 image requires 9 MB of storage while a lossless PNG equivalent needs only 127 kB.

## A Mandelbrot galaxy.

## References.

[1] Gleick, J. (1987) ‘Chaos: Making a New Science’, Viking Books: New York City.

[2] Mandelbrot, B. B. (1982) ‘The Fractal Geometry of Nature’, W. H. Freeman & Co.: New York.

[3] Ogihara, T. (n.d.) 'ToyViewer: Image viewer with utilities'. Available at https://apps.apple.com/us/app/toyviewer/id414298354.

[4] Douady, A., and Hubbard, J. H. (1982) ‘Itération des polynômes quadratiques complexes’, C. R. Acad. Sci. Paris Sér. I Math., 294(3), pp. 123-126.

[5] Khan, J. (2001) ‘The Mandelbrot Set is Connected: a Topological Proof’. Available at https://www.math.brown.edu/jk17/mconn.pdf.
