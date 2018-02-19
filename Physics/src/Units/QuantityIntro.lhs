- 
Oskars kompileringsinsturktioner

Skriv `.lhs`-filer med bird-style `> ` för kod och `< ` för "kod" som inte ska kompileras.

Kompilera med kommandot `pandoc FILNAMN.lhs -f markdown+lhs -t html -o FILNAMN.html -s`

Quantity
========

A *quantity* is fundamental to physics. A *quantity* has a *unit* and a *numerical value* (and a prefix, but we will skip prefixes). Only quantities of the same unit are possible to add. Summing, for instance, a volume and a distance, wouldn't make sense.

There are 7 types of quantities, each with a corresponding SI-unit:

- Length (metre)
- Mass (kilogram)
- Time (seconds)
- Electric current (ampere)
- Temperature (kelvin)
- Amount of substance (mole)
- Luminous intensity (candela)

We will only use the SI-units. Therefore, for instance, "meters" will be exchangable with "length". For this reason we have chosen to name all "units" with the corresponding name of the quantity.

The domain specific language for units will cover units on both *value-level* and *type-level*. Value-level in order to print units nicely. Type-level in order to, already at the time of compilation, make sure no unallowed operations are performed.

The implementation at both levels are very similar. One could choose to only do one implementation, and use `Data.Proxy`, but this is tricky. Using two implementations is lengthier but easier.

Let's start with units on value-level.