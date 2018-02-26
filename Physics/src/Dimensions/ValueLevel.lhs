
Value-level dimensions
======================

From the introduction, two things became apparanent:

1. Given the unit of a quantity, its dimension is known implicitly.
2. If we only care about SI-units, there is a one-to-one correspondence between dimensions and units.

We'll use these facts when implementing dimensions. More precisely, we'll basically don't care about units at all in this text, save for printing. But let's no get ahead of ourselves anymore. This way of treating dimensions and quantites will become clear.

> module Units.ValueLevel
> ( Unit(..)
> , mul
> , div
> , length
> , mass
> , time
> , current
> , temperature
> , substance
> , luminosity
> , one
> )
> where

> import Prelude hiding (length, div)

> len :: (Integral n) => [a] -> n
> len [] = 0
> len (a:as) = 1 + len as

A unit can be seen as a product of the base units, with an individual exponent on each base unit. Since the 7 base units are known in advance, we can design our data type after this fact.

> data Unit = Unit Integer -- Length
>                  Integer -- Mass
>                  Integer -- Time
>                  Integer -- Current
>                  Integer -- Temperature
>                  Integer -- Substance
>                  Integer -- Luminosity

Each field denotes the exponent for the corresponding base unit. If the exponent is `0`, the base unit is not part of the unit. Some examples should clarify.

> length      = Unit 1 0 0 0 0 0 0
> mass        = Unit 0 1 0 0 0 0 0
> time        = Unit 0 0 1 0 0 0 0
> current     = Unit 0 0 0 1 0 0 0
> temperature = Unit 0 0 0 0 1 0 0
> substance   = Unit 0 0 0 0 0 1 0
> luminosity  = Unit 0 0 0 0 0 0 1

> velocity     = Unit 1 0 (-1) 0 0 0 0
> acceleration = Unit 1 0 (-2) 0 0 0 0

Velocity is `m/s` or equivalently `m^1*s^-1`. This explains why the exponents are as above.

Units can be multiplied and divided. Velocity is, as we just saw, a division between length and time. Multiplication and division of units are performed as if they were regular numbers, or variables holding numbers, and hence they follow the power laws. That is, to multiply, the exponents of the two numbers are added, and to divide, the exponents are subtracted.

> mul :: Unit -> Unit -> Unit
> (Unit le1 ma1 ti1 cu1 te1 su1 lu1) `mul` (Unit le2 ma2 ti2 cu2 te2 su2 lu2) =
>   Unit (le1+le2) (ma1+ma2) (ti1+ti2) (cu1+cu2) (te1+te2) (su1+su2) (lu1+lu2)

> div :: Unit -> Unit -> Unit
> (Unit le1 ma1 ti1 cu1 te1 su1 lu1) `div` (Unit le2 ma2 ti2 cu2 te2 su2 lu2) =
>   Unit (le1-le2) (ma1-ma2) (ti1-ti2) (cu1-cu2) (te1-te2) (su1-su2) (lu1-lu2)

It's now possible to construct units in the following way.

> velocity' = length `div` time
> area      = length `mul` length
> force     = mass   `mul` acceleration
> momentum  = force  `mul` time

A "unit" we so far haven't mentioned is the *scalar*, which shows up when working with, for example, coefficients of friction. It's unitless since it arises from division of two equal units. `f = u * N <-> u = f / N`.

> one  = force `div` force
> one' = Unit 0 0 0 0 0 0 0

Pretty-printer
--------------

The purpose of units on value-level is to be able to print 'em nicely. So let's create a pretty-printer.

> showUnit :: Unit -> String
> showUnit (Unit le ma ti cu te su lu)
>   | null negStrs = posStr
>   | otherwise    = posStr ++ "/" ++ negStr'
>   where
>     paired  = [("m",le), ("kg",ma), ("s",ti), ("A",cu),
>                ("K",te), ("mol",su), ("cd",lu)]
>     pos     = filter (\(_, exp) -> exp >  0) paired
>     neg     = filter (\(_, exp) -> exp <  0) paired
>     neg'    = map (\(u, exp) -> (u, -exp)) neg

>     f (u,1) = u
>     f (u,n) = u ++ "^" ++ show n

>     posStrs = map f pos
>     negStrs = map f neg'
>     posStr  = if null pos
>               then ""
>               else foldl (\strs str -> str ++ "*" ++
>                    strs) (head posStrs) (tail posStrs)
>     (left, right) = if len negStrs > 1
>                     then ("(", ")")
>                     else ("", "")
>     negStr = if null negStrs
>              then ""
>              else foldl1 (\strs str -> str ++ "*" ++ strs) negStrs
>     negStr' = left ++ negStr ++ right
> 
> instance Show Unit where
>   show = showUnit

Now units are printed quite pretty in GHCi.

< ghci> momentum
< kg*m/s

The result from this section has been the ability to multiply, divide and print units prettily. The next natural step would be to create a data type for quantities. However, we will first implement units on type-level.
