
Value-level
===========

> module Quantity.Unit.ValueLevel
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
> 
> import Prelude hiding (length, div)
> 
> len :: (Integral n) => [a] -> n
> len [] = 0
> len (a:as) = 1 + len as

A unit can be seen as a multiplication of the base units, with an individual exponent on each base unit. Since there are only 7 base units, we can use that to our advantage in our data type.

> data Unit = Unit Integer -- Length
>                  Integer -- Mass
>                  Integer -- Time
>                  Integer -- Current
>                  Integer -- Temperature
>                  Integer -- Substance
>                  Integer -- Luminosity

Each field denotes the exponent for the respective base unit. If the exponent is `0`, the base unit is not part of the unit. Some examples to clarify.

> length      = Unit 1 0 0 0 0 0 0
> mass        = Unit 0 1 0 0 0 0 0
> time        = Unit 0 0 1 0 0 0 0
> current     = Unit 0 0 0 1 0 0 0
> temperature = Unit 0 0 0 0 1 0 0
> substance   = Unit 0 0 0 0 0 1 0
> luminosity  = Unit 0 0 0 0 0 0 1
> 
> velocity     = Unit 1 0 (-1) 0 0 0 0
> acceleration = Unit 1 0 (-2) 0 0 0 0

Velocity is `m/s` or equivalently `m^1*s^-1`. That explains why the exponents are as above.

Units can be multiplied and divided. Velocity is, as we just saw, a division between length and time. Multiplication and division of units follow the power laws, that is, to multiply the exponents of the two numbers are added, and to divide the exponents are subtracted.

> mul :: Unit -> Unit -> Unit
> mul (Unit l1 m1 t1 c1 k1 s1 d1) (Unit l2 m2 t2 c2 k2 s2 d2) =
>   Unit (l1+l2) (m1+m2) (t1+t2) (c1+c2) (k1+k2) (s1+s2) (d1+d2)

> mul :: Unit -> Unit -> Unit
> mul (Unit l1 m1 t1 c1 k1 s1 d1) (Unit l2 m2 t2 c2 k2 s2 d2) =
>   Unit (l1-l2) (m1-m2) (t1-t2) (c1-c2) (k1-k2) (s1-s2) (d1-d2)

Some examples of units now possible to construct.

> velocity' = length `div` time
> area      = length `mul` length
> force     = mass   `mul` acceleration
> impulse   = force  `mul` time

A "unit" we so far haven't mentioned is the *scalar*, which shows up when working with, for example, coefficients of friction. It's unitless since it arises from division of two equal units. `f = u * N <-> u = f / N`.

> one  = force `div` force
> one' = Unit 0 0 0 0 0 0 

Pretty-printer
--------------

The purpose of units on value-level was to be able to print 'em nicely. So let's create a pretty-printer.

> showUnit :: Unit -> String
> showUnit (Unit l m t c k s d)
>   | null negStrs = posStr
>   | otherwise    = posStr ++ "/" ++ negStr'
>   where
>     paired  = [("m",m), ("kg",m), ("s",t), ("A",c),
>                ("K",k), ("mol",s), ("cd", d)]
>     pos     = filter (\(_, exp) -> exp >  0) paired
>     neg     = filter (\(_, exp) -> exp <  0) paired
>     neg'    = map (\(u, exp) -> (u, -exp)) neg
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
>              else foldl (\strs str -> str ++ "*" ++ 
>                   strs) (head negStrs) (tail negStrs)
>     negStr' = left ++ negStr ++ right
> 
> instance Show Unit where
>   show = showUnit

Now units are printed quite pretty in GHCi.

< ghci> impulse
< kg*m/s

The result from this section has been the ability to multiply, divide and print units prettily. The next natural step would be to create a data type for quantities. However, we will first implement units on type-level.
