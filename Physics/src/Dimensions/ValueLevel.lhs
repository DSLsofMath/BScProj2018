
Value-level dimensions
======================

From the introduction, two things became apparanent:

1. Given the unit of a quantity, its dimension is known implicitly.
2. If we only care about SI-units, there is a one-to-one correspondence between dimensions and units.

We'll use these facts when implementing dimensions. More precisely, "length" and "metre" will be interchangable, and so on.

> module Dimensions.ValueLevel
> ( Dim(..)
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

A dimension can be seen as a product of the base dimensions, with an individual exponent on each base dimension. Since the 7 base dimensions are known in advance, we can design our data type using this fact.

> data Dim = Dim Integer -- Length
>                Integer -- Mass
>                Integer -- Time
>                Integer -- Current
>                Integer -- Temperature
>                Integer -- Substance
>                Integer -- Luminosity

Each field denotes the exponent for the corresponding base dimension. If the exponent is `0`, the base dimension is not part of the dimension. Some examples should clarify.

> length      = Dim 1 0 0 0 0 0 0
> mass        = Dim 0 1 0 0 0 0 0
> time        = Dim 0 0 1 0 0 0 0
> current     = Dim 0 0 0 1 0 0 0
> temperature = Dim 0 0 0 0 1 0 0
> substance   = Dim 0 0 0 0 0 1 0
> luminosity  = Dim 0 0 0 0 0 0 1

> velocity     = Dim 1 0 (-1) 0 0 0 0
> acceleration = Dim 1 0 (-2) 0 0 0 0

Velocity is $m/s$ or equivalently $m^1*s^{ -1 }$. This explains why the exponents are as above.

Noticed how we used "m" (for metre) for implicitly refering to the dimension "length"? It's quite natural to work this way.

Multiplication and division
---------------------------

Dimensions can be multiplied and divided. Velocity is, as we just saw, a division between length and time. Multiplication and division of dimensions are performed as if they were regular numbers, or variables holding numbers, and hence they follow the power laws. That is, to multiply, the exponents of the two numbers are added, and to divide, the exponents are subtracted.

> mul :: Dim -> Dim -> Dim
> (Dim le1 ma1 ti1 cu1 te1 su1 lu1) `mul` (Dim le2 ma2 ti2 cu2 te2 su2 lu2) =
>   Dim (le1+le2) (ma1+ma2) (ti1+ti2) (cu1+cu2) (te1+te2) (su1+su2) (lu1+lu2)

> div :: Dim -> Dim -> Dim
> (Dim le1 ma1 ti1 cu1 te1 su1 lu1) `div` (Dim le2 ma2 ti2 cu2 te2 su2 lu2) =
>   Dim (le1-le2) (ma1-ma2) (ti1-ti2) (cu1-cu2) (te1-te2) (su1-su2) (lu1-lu2)

It's now possible to construct dimensions in the following way.

> velocity' = length `div` time
> area      = length `mul` length
> force     = mass   `mul` acceleration
> momentum  = force  `mul` time

A dimension we so far haven't mentioned is the *scalar*, which shows up when working with, for example, coefficients of friction. It's dimensionless since it arises from division of two equal dimensions. The case of coefficients of friction

\begin{align}
  F_{friction} = \mu * F_{normal} \iff \mu = \frac{F_{friction}}{F_{normal}}
\end{align}

> one  = force `div` force
> one' = Dim 0 0 0 0 0 0 0

Pretty-printer
--------------

The purpose of value-level dimensions is to be able to print 'em nicely. So let's create a pretty-printer.

> len :: (Integral n) => [a] -> n
> len [] = 0
> len (a:as) = 1 + len as

> showDim :: Dim -> String
> showDim (Dim le ma ti cu te su lu)
>   | null negStrs = posStr
>   | otherwise    = posStr ++ "/" ++ negStr'
>   where
>     paired  = [("m",le), ("kg",ma), ("s",ti), ("A",cu),
>                ("K",te), ("mol",su), ("cd",lu)]
>     pos     = filter (\(_, exp) -> exp >  0) paired
>     neg     = filter (\(_, exp) -> exp <  0) paired
>     neg'    = map (\(d, exp) -> (d, -exp)) neg
> 
>     f (u,1) = u
>     f (u,n) = u ++ "^" ++ show n
> 
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

> instance Show Dim where
>   show = showDim

Now dimensions are printed quite pretty in GHCi.

< ghci> momentum
< kg*m/s

Note that the SI-unit of the dimensions is used for printing.

The result from this section has been the ability to multiply, divide and print dimensions prettily. The next natural step would be to create a data type for quantities. However, we'll first implement type-level dimensions.
