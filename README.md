# fractals
Messing about plotting fractals with Haskell.

![Main bulb of the Mandelbrot set](https://github.com/ncke/fractals/blob/c817a10e9ed66f838480e6a9e04c54699479f8a0/examples/mainbulb-900x900.png)
## What are fractals?
Fractals are a kind of shape. Unlike ordinary shapes, like squares and circles, a fractal has the property of self-similarity. This means that no matter how deeply you zoom in on some part of the fractal, the original motif continues to emerge at an ever smaller scale.

One of the reasons why fractals are mathematically interesting is because self-similarity can also be observed in many natural shapes. For example, a satellite view of a country’s coastline will have bays and promontories. But zooming-in reveals a more jagged edge with smaller bays and inlets. This repetition of coastal features at different scales is an example of self-similarity that was created by a natural process. Understanding fractals helps us to understand natural shapes in a different way.

The intricacy in the coastline is created by the relatively simple process of coastal erosion — the interaction of the waves and wind at the shoreline. The shape emerges from macroscopic features such as rock types and prevailing winds, but much of the complexity emerges because previous weathering influences future patterns — perhaps channelling water towards or away from a particular section. Even small changes inside these feedback loops will compound across geological timescales to create significant differences. More dynamic systems, such as the weather, are even more sensitive so that it is difficult to forecast more than a few days or weeks in advance.  This sensitivity is a part of chaos theory known as the Butterfly Effect, thanks to Edward Lorenz’s suggestion that the tiny perturbations caused by a butterfly’s wing could (at least in theory) create a chain of atmospheric events ending in a tornado several weeks later.

Fractals and chaos theory have changed the way that we think about engineered systems. We are no longer surprised when complex behaviour emerges from interacting components, particularly when a feedback loop is involved. More importantly, it is also fun to generate fractals for ourselves and look at them using our computers.

There’s a lot more information about chaos theory and fractals in James Gleick’s book ‘Chaos: Making a New Science’, which is a bit of a classic [1].

## The Mandelbrot set.
We have seen that fractals are shapes that are self-similar at many scales, a property often seen in nature. The mathematician [Benoit B. Mandelbrot](https://en.wikipedia.org/wiki/Benoit_Mandelbrot) described many other naturally occurring fractals [2]. He also gave his name to one of the most well-known mathematical fractals: the Mandelbrot Set.

The Mandelbrot set is a distinctive collection of points in the complex plane. The complex plane exists because of complex numbers, and complex numbers exist largely to answer the question of how to find the square root of negative one.

For our purposes, the most important thing to know is that a complex number consists of two parts: a real part and an imaginary part. So we can write a complex number such as:

2.5 + 3.2i

The real part of the number is 2.5, and the imaginary part is 3.2. With this in mind we can now make two simplifying observations.

Firstly, the complex plane is a two-dimensional plane. Each complex number sits at its own position that is determined by the real part, which is typically measured along the x-axis, and the imaginary part, which is measured along the y-axis. Our computer screens are organised in the same way, which is ideal because we can easily map regions of the complex plane into images.

The second observation has to do with the fact that the complex plane extends infinitely in all directions; the real and imaginary parts can be as large as you like in both the positive and negative direction. Computer images are not infinite, so this could be inconvenient. The good news, however, is that no point in the Mandelbrot set lies more than two units from the origin at 0 + 0i. So that’s okay then.

Figure 1 shows the Mandelbrot set in the complex plane and highlights some arbitrary locations. Points that are inside the set are conventionally coloured black, the membership test is described below. In this diagram the points that are outside the set are coloured white, later on we discuss how to shade these points to produce colourful plots.









## References.

[1] Gleick, J. (1987) ‘Chaos: Making a New Science’, Viking Books: New York City.
[2] Mandelbrot, B. B. (1982) ‘The Fractal Geometry of Nature’, W. H. Freeman & Co.: New York.
