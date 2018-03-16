
Introduction
============

> module Dimensions.Intro where

This chapter is about dimensions, quantities and units. What's the difference?

- A **dimension** is "what type of thing something is". For instance, *length* is a thing, *area* is a thing and *velocity* is a thing. Furthermore, they are *different* things.
- A **quantity** is something that can be *quantified*, i.e, something that can have a number associated with it. Examples are the distance between Stockholm and Gothenburg, the area of a soccer field and the speed of light.
- A **unit** is a certain magnitude of something. 1 metre is for instance approximatly the length of your arm.

What's the relation between these? A *quantity* has a *dimension*. The number describing the distance between Stockholm and Gothenburg is of the type length. A *quantity* also has a *unit* that relates the number to a known definite distance. The unit of a quantity must describe the dimension of the quantity. It's not possible to describe a distance with joule. However, describing a distance is possible with both metres and inches. Those are two different units describing a quantity of the same dimension.

The dimension of a quantity is often implicitly understood given its unit. If I have a rope of 1 metre, you know it's a length I'm talking about.

There are 7 *base dimensions*, each with a corresponding SI-unit.

![The 7 base dimensions](Base_dimensions.png){.float-img-right}

- Length (metre)
- Mass (kilogram)
- Time (seconds)
- Electric current (ampere)
- Temperature (kelvin)
- Amount of substance (mole)
- Luminous intensity (candela)

The outline of this chapter is to first introduce dimensions on *value-level* (to print them nicely). Then we'll do dimensions on *type-level* (to only permit legal operations). And finally we'll combine those results to create a data type for quantities.

In science SI-units are prefered over all other units. Therefore we'll only care about SI-units. Since we now have a one-to-one correspondence between dimensions and units, only one is really needed!

Let's start with value-level dimensions.
