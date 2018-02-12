
Haskell klagar om man har # som första tecken...
 ## Värdenivå

Varför ha enheter på värdenivå (utöver typnivå)? Anledningen är att kunna pretty-printa uttryck. Implementationen för uttryck på värdesnivå är snarlik den på typnivå. Man hade kunna nöja sig med en implementation och använda sig av `Data.Proxy` men det blir krånligt. Detta sätt är längre men lättare.

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

TODO: Introducera de 7 SI-enheterna

> data Unit = Unit Integer -- Length
>                  Integer -- Time
>                  Integer -- Mass

Implementation av multiplikation och division av enheter. TODO: Förklara närmare.

> mul :: Unit -> Unit -> Unit
> mul (Unit l1 t1 m1) (Unit l2 t2 m2) = Unit (l1+l2) (t1+t2) (m1+m2)

> div :: Unit -> Unit -> Unit
> div (Unit l1 t1 m1) (Unit l2 t2 m2) = Unit (l1-l2) (t1-t2) (m1-m2)

Några exempel på enheter.

> length = Unit 1 0 0
> time   = Unit 0 1 0
> mass   = Unit 0 0 1
> area   = Unit 2 0 0
> velocity = Unit 1 (-1) 0

