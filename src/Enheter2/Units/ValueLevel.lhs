
Värdenivå
=========

> module Units.ValueLevel
> ( Unit(..)
> , mul
> , div
> , length
> , time
> , mass
> )
> where
> 
> import Prelude hiding (length, div)
> import Helper

En enhet kan ses som en multiplikation av grundenheterna med en exponent. Eftersom det bara finns 5 grundenheter bestämt från början utnyttjar vi det i vår datatyp för enheter.

> data Unit = Unit Integer -- Length
>                  Integer -- Time
>                  Integer -- Mass
>                  Integer -- Temperature
>                  Integer -- Amount of substance

Vardera fält anger exponenten för respektive grundenhet. Är exponenten `0` så är grundenheten inte med. Några exempel bör förtydliga.

> length      = Unit 1 0 0 0 0
> time        = Unit 0 1 0 0 0
> mass        = Unit 0 0 1 0 0
> temperature = Unit 0 0 0 1 0
> substance   = Unit 0 0 0 0 1
> 
> velocity     = Unit 1 (-1) 0 0 0
> acceleration = Unit 1 (-2) 0 0 0

Hastighet är `m/s` eller `m^1*s^-1*. Det motiverar varför exponenterna blev som de blev.

Enheter kan multipliceras och divideras. Hastighet är som vi precis såg en division mellan längd och tid. Multiplikation och division av enheter följer potensreglerna för de vanliga talen, det vill säga, vid multiplikation av två enheter adderas exponenterna och vid division subtraheras de.

> mul :: Unit -> Unit -> Unit
> mul (Unit l1 t1 m1 k1 s1) (Unit l2 t2 m2 k2 s2) =
>   Unit (l1+l2) (t1+t2) (m1+m2) (k1+k2) (s1+s2)

> div :: Unit -> Unit -> Unit
> div (Unit l1 t1 m1 k1 s1) (Unit l2 t2 m2 k2 s2) =
>   Unit (l1-l2) (t1-t2) (m1-m2) (k1-k2) (s1-s2)

Några exempel på enheter vi nu kan konstruera.

> velocity' = div length time
> area      = mul length length
> force     = mul mass acceleration
> impulse   = mul force time

Pretty-printer
--------------

Själva syftet med att ha enheter på värdesnivå var att ha en pretty-printer. Så här gör vi en.

> showUnit :: Unit -> String
> showUnit (Unit l t m k s)
>   | null negStrs = posStr
>   | otherwise    = posStr ++ "/" ++ negStr'
>   where
>     paired  = [("m",l),("s",t),("kg",m),
>                ("K",k),("mol",s)]
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

Nu visas enheter prydligt när man skriver dem i GHCi.

< ghci> impulse
< kg*m/s


