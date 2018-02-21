
TiltedPlane
===========

> module TiltedPlane
> (
> )
> where

Lite kladd
----------

Ett lutande plan består av en låda som ligger på en triangel med en viss vinkel. Det finns egenskaper som massor, friktionskoecienter, hastigheter m.m. Alla dessa hänger ihop med olika ekvationer. Ett DSL innehåller alla dessa ekvationer. Och sedan så kan man manipulera ekvationerna för att få fram den sökta variabeln (ev. som funktion av något annat).
 a
 
Enklaste fallet är ingen friktion och en låda som man bara släpper och som glider ner. Beräkna accelerationen längst med planet.

a = sin theta * g, borde gälla, men svårt att avgöra utan en bild. Hitta så enkla/direkta samband som möjligt mellan "intressanta" tal som vinkel och acceleariton och rörelseenergi. a = sin theta * g är att föredra framför vf^-vi^2 = 2*a*x.
 
< -- "tilted plane"
< data TP = Angle
<         | Acceleration
   
< -- Operation
< data Op = Variable TP
<         | Sin Op
<         | Mul Op Op
<         | GA -- Gravitantional acceleration
<         | SinInv Op
<         | Div Op Op
   
< -- Likhet mellan två operationer/uttryck
< data Equ = Equ Op Op
   
< -- Man har en massa olika samband av detta slag
< equality1 = Equ (Variable Acceleration) (Sin (Variable Angle) `Mul` GA)
    
Sambanden vill man kunna manipulera, precis som man manipulerar ekvationer vanligtvis. Istället för att ha någon smart funktion som gör det åt en, så kanske det räcker att säga att en likhet man fått fram från en annan gäller om den kan QuickCheckas. Den första likheten ses som en funktion som ger ett värde, som man stoppar in i den andra likheten och ser nu att den fortfarande gäller.

Mer strukturerat
----------------

Ett lutande plan består av en låda på en lutande triangel. Om vi tänker på ett enkelt sådant system, utan friktion och begynnelsehastighet, så finns följande egenskaper hos det.

| Egenskap                  | Beteckning |
|---------------------------|------------|
| Vinkel                    | v          |
| Acceleration längs planet | a          |
| Tyngdacceleration         | g          |

Dessa egenskaper förhåller sig till varandra med ekvationer. Exempelvis `a = g * sin v`. I en lutande-plan uppgift kan man bli ombedd att hitta en av dessa egenskaper givet de andra egenskaperna. Har man den ekvationen och får `v` är det enkelt. Tvärtom blir lite lurigare, eftersom man här får omforma ekvationen till `v = sin^-1 (a / g)`.

> data TP = V
>         | A
>         | G

> data Expr = Expr `Mul` Expr
>           | Expr `Div` Expr
>           | Var TP
>           | Sin  Expr
>           | Sin1 Expr

> data Equ = Expr `Equals` Expr

Sambandet i grundform kan då kodas upp i Haskell som

> lhs1 = Var A
> rhs1 = Var G `Mul` Sin (Var V)
> equation1 = lhs1 `Equals` rhs1

Ekvationen `a = g * sin v` och dess omformning `v = sin^-1 (a / g)` är ekvivalenta. Att koda upp båda är därför onödigt.

Uttryck systemet som ett gäng ekvationer. Man matar in data till systemet, och dessa ska klara en quickcheck.

> (===) :: Double -> Double -> Bool
> a === b = abs (a-b) < 0.001

> eval :: Double -> Double -> Expr -> Double
> eval a v (e1 `Mul` e2) = eval a v e1 * eval a v e2
> eval a v (e1 `Div` e2) = eval a v e1 / eval a v e2
> eval a v (Sin  e) = sin (eval a v e)
> eval a v (Sin1 e) = asin (eval a v e)
> eval a v (Var A) = a
> eval a v (Var V) = v
> eval a v (Var G) = 9.82

> check :: Double -> Double -> Equ -> Bool
> check a v (e1 `Equals` e2) = eval a v e1 === eval a v e2