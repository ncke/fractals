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

One of the reasons why fractals are mathematically interesting is because self-similarity can also be observed in many natural shapes. For example, a satellite view of a country’s coastline will have bays and promontories. But zooming-in reveals a more jagged edge with smaller bays and inlets. This repetition of coastal features at different scales is an example of self-similarity that was created by a natural process. Understanding fractals helps us to understand natural shapes in a different way.

The intricacy in the coastline is created by the relatively simple process of coastal erosion — the interaction of the waves and wind at the shoreline. The shape emerges from macroscopic features such as rock types and prevailing winds, but much of the complexity emerges because previous weathering influences future patterns — perhaps channelling water towards or away from a particular section. Even small changes inside these feedback loops will compound across geological timescales to create significant differences. More dynamic systems, such as the weather, are even more sensitive so that it is difficult to forecast more than a few days or weeks in advance.  This sensitivity is a part of chaos theory known as the Butterfly Effect, thanks to [Edward Lorenz](https://en.wikipedia.org/wiki/Edward_Norton_Lorenz))’s suggestion that the tiny perturbations caused by a butterfly’s wing could (at least in theory) create a chain of atmospheric events ending in a tornado several weeks later.

Fractals and chaos theory have changed the way that we think about engineered systems. We are no longer surprised when complex behaviour emerges from interacting components, particularly when a feedback loop is involved. More importantly, it is also fun to generate fractals for ourselves and look at them using our computers.

There’s a lot more information about chaos theory and fractals in James Gleick’s book ‘Chaos: Making a New Science’, which is a bit of a classic [1].

## The Mandelbrot set.
We have seen that fractals are shapes that are self-similar at many scales, a property sometimes seen in nature. The mathematician [Benoit B. Mandelbrot](https://en.wikipedia.org/wiki/Benoit_Mandelbrot) described many other naturally occurring fractals [2]. He also gave his name to one of the most well-known mathematical fractals: the Mandelbrot Set.

The Mandelbrot set is a distinctive collection of points in the complex plane. The complex plane exists because of complex numbers, and complex numbers exist largely to answer the question of how to find the square root of negative one.

For our purposes, the most important thing to know is that a complex number consists of two parts: a real part and an imaginary part. The idea of a number being imaginary is to do with that square root, but we won’t go into that now. We just  need to know how to write a complex number like this:

2.5 + 3.2i

The real part of the number is 2.5, and the imaginary part is 3.2. With this in mind we can now make two simplifying observations.

Firstly, the complex plane has two dimensional. Each complex number sits at a position that is determined by the real part, which is normally measured along the x-axis, and the imaginary part, which is measured along the y-axis. Computer screens are organised in the same way, which is ideal because we can easily map regions of the complex plane into images.

The second observation has to do with the fact that the complex plane extends infinitely in all directions; the real and imaginary parts can be as large as you like in both the positive and negative sense. Computer images are not infinite, so this could be inconvenient. The good news, however, is that no point in the Mandelbrot is more than two units from the origin at 0 + 0i. So that’s okay then.

Figure 1 shows the Mandelbrot set in the complex plane. Points that are members of the set are conventionally coloured black; and the membership test is described below. In this figure the points that are outside the set are shaded red, later on we discuss how to shade these points to produce more colourful plots.

## How it works.

👉 **Disclaimer.** I’m not an Haskell expert, I’m a beginner. I’m teaching myself Haskell because I think that functional languages are ace and because I think that learning to express yourself in different programming languages helps you to write better code overall.

The `Main` function implements a four-step process, shown in Figure 2 below.

<img src="https://github.com/ncke/fractals/blob/bb3a727447ac90038eb9ac2508f24b194f7baed5/resources/figure-2.png" width=500>

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

Secondly, we perform the plot itself. At this stage we aren’t thinking about an image or colours. Instead, we concentrate on the mathematics of the fractal itself at each point in the region of interest.

We have a polymorphic data type called a `Tile a` that stores all of the values in a rectangular area. And, in fact, we have a data type called `Region` that we use to specify exactly where that rectangular area is and its size. Without going into too much detail yet, we now have a populated  `Tile Int`.

In the third stage we shade each of those points. We go back over the tile and generate RGB values which now gives us a `Tile (Int, Int, Int)`.

The fourth and final step is to render the RGB tile into an image that we can view. For simplicity, and to avoid dependencies, we use the P3 (.ppm) format. P3 is text-based and uncompressed. This is a little hungry for disk space, but this is not a project about image compression. On a Mac, the Preview app will handle P3 images or we can use a utility like ToyViewer [3].

## Plotting.
The most interesting parts of the fractal-producing process are plotting and shading. Let’s discuss plotting first; this is organised in the `Plot` module, but most of the work happens in the `Tile` and `Algorithm` modules.

#### More about tiles.

We already know that we are going to generate a fully populated `Tile Int`.  The tile data type isn’t just a container for Ints. Instead it knows that it is a tile — some rectangular region of a plane. So it has a `region` property to describe the location, and an `elements` property to hold the actual values. 

When you think about it, the properties for the region and the elements in a tile are inextricably linked. We can’t just take the elements from one tile and plonk them together with the region from another tile. At least, if we did do that then the outcome would not be a valid reflection of the reality that we are trying to model. To prevent this, `Tile` is implemented as an abstract data type. We use encapsulation to hide away implementation details as well as to maintain the conceptual purity of a tile. For example, we don’t expose how our storage of the elements is actually arranged. It’s a list of lists, but another programmer doesn’t need to know that, they can just use the `element` function to query for a value at a particular x-y location.

More significantly, we abstract away the process for creating a `Tile` so that the relationship between the region and its elements is enforced. Instead of exposing the initialiser, we provide a `generate` function that must be used to create tiles. The type signature looks like this:

`generate :: Region -> (Int -> Int -> a) -> Tile a`

We pass in a region, and then we pass in a ‘strategy’ for generating the elements of that region; and, in return, the `generate` function will use the strategy to give us a fully populated tile. The tile takes responsibility for running the strategy for each and every location in the region, so it's not possible to mix up the elements for a different region.

The strategy itself has the signature `(Int -> Int -> a)`, this type has actually been given the alias `Algorithm` but we don’t use that here to avoid a dependency cycle. The two integers are an x-y position inside the tile, it’s the job of the strategy implementation to take that position and return the corresponding value of type `a`. One thing to note here is that the tile doesn’t know about the coordinate system that complex numbers use (it's more general than that). As far as the tile is concerned, every element occurs at a 2D integer point, like (5, 14) for example. It’s up to the strategy algorithm to map this to whatever coordinate system it wants to use, we will see how that is done next.

#### A strategy for the Mandelbrot set.

So how does this work for the Mandelbrot set in the complex plane. The `Tile` is going to call our plotting strategy with some x-y Ints, like (5, 14), how do we handle that?

The first thing to do is convert the integer coordinates that we have been given by the tile into a position in the complex plane. We have the `Configuration` instance to guide us. Figure 3 below shows the general idea. We know, from the configuration, the complex coordinate for the lower right hand corner, in this example it is -2.0 - 1.25i and the size of the region is 2.0 + 2.0i; and we are also given the overall image size in pixels. So for any given x-y pair we can use linear interpolation to work out the corresponding position in the complex plane.

![Linear interpolation to the complex plane](https://github.com/ncke/fractals/blob/ef970f3ecec8fe230ed4d77ca9b98b8f278729eb/resources/figure-3.png)

**Figure 3.** (a) How the tile sees it, a 20x20 grid of points is shown with (5, 14) highlighted. (b) How the Mandelbrot strategy uses the configuration data structure to map onto the complex plane. For the real part, 5 * 2.5 / 20 = 0.625, and for the imaginary part 14 * 2.5i / 14 = 1.75i. Adding this offset to the origin at the lower left gives us -1.375 + 0.5i.

Now what? We’ve got our complex number, how do we know whether it is inside the Mandelbrot set or not? The equation is shown below at Equation 1. Given the intricacy of the Mandelbrot set, the suprising thing about Equation 1 is how simple it is. That is all it takes to set up a feedback loop that will create an enormously complex shape (infinitely complex, in fact). This chaos from concision is one of the reasons why the Mandelbrot set is admired by mathematicians.

<img src="https://github.com/ncke/fractals/blob/4d033336e348e9b7d97d5a59e4586222f0a74702/resources/equation-1.png" width=300>

**Equation 1.** The Mandelbrot set.

We iterate this equation so that z<sub>0</sub> produces z<sub>1</sub>, which produces z<sub>2</sub>, which produces z<sub>3</sub>, and so forth. The process of iteration always starts at the origin, so z<sub>0</sub> is always 0.0 + 0.0i. And c is kept constant throughout, it's the point of interest. In our example, c is -1.375 + 0.5i. We carry on iterating to see if z<sub>n</sub> is ever more than 2.0 units away from the origin. If that happens then we know for sure that the point (c) is not in the Mandelbrot set. If a point is in the set then we could carry on iterating forever, z<sub>n</sub> will always orbit the origin and will never fly off. But iterating forever would take a long time and we're impatient to see our fractal. So we set an arbitrary maximum limit to the number of iterations that we will make before we just assume that the point is in orbit and therefore in the set. In the plot for Figure 1 that limit was 250 iterations.

Remember that, for plotting, we want to get a `Tile Int` data type. Where does the Int come from? If we hit the maximum limit for the number iterations then we populate that the location with a 0 to say that the point is inside the Mandelbrot set. Otherwise we return the number of iterations that were achieved before the z<sub>n</sub> flew out of orbit beyond 2.0. We could do something a bit more fancy than just use 0 to denote a point in the set, but this is a commonly used representation and it keeps things simple.
  
Figure 4 shows how this all works out for our example point at -1.375 + 0.5i. Note that complex numbers have their own special way of performing multiplication, we use `Data.Complex` to handle this for us.
  
<img src="https://github.com/ncke/fractals/blob/4d033336e348e9b7d97d5a59e4586222f0a74702/resources/figure-4.png" width=600>

**Figure 4.** Tracking successive iterations for the point c = -1.375 + 0.5i until the 2.0-unit radius circle is escaped.

In figure 4 we can see that the point escapes after only three iterations, so we can say that this point is not a member of the Mandelbrot set. We return `n = 3` in this case. Speaking loosely, `n` is a kind of measure of how 'close' the point is to the set. It provides the basis for shading the chart, as we'll see later.
  
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

That last line determines whether the point has left orbit, it has a little trick. We use the Pythagorean formula to calculate displacement from the origin, but square roots can be expensive to compute. We leave the number as a square and compare with 4.0 (which is 2.0<super>2</super>) so it's the same test. This optimisation is necessary. Plotting an 800 x 800 pixel fractal involves testing 640,000 points in the complex plane. Suppose we set the iteration maximum at 250, then, in a worst-case scenario, there will be 160 million iterations. In Big O-notation, we would say that the algorithm for plotting an n x n pixel fractal is O(n<sup>2</sup>) which is pretty fierce considering that we want nice big images with lots of pixels.

Actually though, there's a much better trick that we can also employ to speed things along. Let's talk about that next.

## Improving plotting performance.

## Shading.

## Improving the shader.

## Examples.

## References.

[1] Gleick, J. (1987) ‘Chaos: Making a New Science’, Viking Books: New York City.

[2] Mandelbrot, B. B. (1982) ‘The Fractal Geometry of Nature’, W. H. Freeman & Co.: New York.

[3] ToyViewer.
