
TiltedPlane
===========

> module TiltedPlane
> (
> )
> where

Ett lutande plan består av en låda på en lutande triangel. I ett sådant "system" finns egenskaper som friktion, massor, krafter som drar m.m.

Uppgifter om lutande plan kan ha många former. T.ex. en låda som glider ner och som samtidigt dras uppåt i, eller att triangeln accelereras för att förhindra lådan att glida ner o.s.v.

De fysikalsiska ekvationerna är såklart gemensamma för alla varianter.

Vi kommer skapa några DSLs för olika varianter av problemet. I varje variant finns ett gäng ekvationer som beskriver samband mellan ingående variabler. Ett typiskt sätt att variera en variant är att ge alla värden utom ett, och att det sakande är olika, och beräkna det sakande. (Det kan också vara att skriva som en funktion av några grejer, men samma princip gäller. Man behandlar ogivna variabler som ett värde och räknar som vanligt)

Det handlar alltså om att hålla koll på vilka ekvationer som gäller och hur man använder dom för att lösa ut den okända.

evaluera sambanden till funktioner. svaret är en komposition av funktioner. Men detta kanske mer skulle vara ett DSL för ekvationslösning... Även om sådant också är relevant för fysik. Ide till: Kollar på varje uttryck/term o.s.v. om någon obekant finns i den. Gör "invers" operation (t.ex. div på mul) för att få ut den.

En variant
----------

I denna variant är triangeln stillastående, och lådan glider neråt. En sne kraft drar i den. Det är friktion mellan lådan och triangeln.

<img src="tp_1_1.png" alt="" style="width: 600px;"/>

- $a$ är acceleraion av lådan nerför planet.
- $m$ är massan av lådan.
- $N$ är normalkraften på lådan.
- $G$ är tyngdkraften på lådan.
- $f$ är friktionskraften.
- $\mu$ är friktionskoeffcienten mellan lådan och triangeln.
- $F$ är den snea kraften som drar uppåt.
- $\alpha$ är vinkeln mellan horisontalplanet och den snea kraften.
- $\theta$ är vinkeln på triangelns plan och horisontalplanet.

Som väntat finns det ett gäng samband mellan dem. Positiv riktning är höger och uppåt. Krafterna är definerade i den riktning de är på i bilden. Nettokraften är definerad åt höger och uppåt.

Vi delar upp alla krafter i komposanter. En längs med triangelpanet (subskript p) och en vinkellrätt (subskript n). Vi skapar också en hjälpvinkel.

\begin{align}
  \gamma &= \alpha - \beta \\
  F_p &= F * cos \gamma \\
  F_n &= F * sin \gamma \\
  G_p &= G * sin \beta \\
  G_n &= G * cos \beta
\end{align}

Bilden bör övertyga om de två sista sambanden.

<img src="tp_1_2.png" alt="" style="width: 600px;"/>

Vi ritar en ny bild med de komposantuppdelade krafterna.

<img src="tp_1_3.png" alt="" style="width: 600px;"/>

Och här är sambanden som nu finns. Vad som räknas som positiv rikning finns angivit i figuren.

\begin{align}
  f &= N * \mu \\
  N + F_n &= G_n \\
  G_p - f - F_p &= F_{netto} \\
  a * m &= F_{netto} \\
\end{align}

Låta oss koda upp dessa samband.

> data Property = Alpha
>               | Beta
>               | Gamma
>               | F
>               | Fp
>               | Fn
>               | G
>               | Gp
>               | Gn
>               | Ff
>               | N
>               | M
>               | A
>               | Fnet  -- The net force
>               | U     -- The coefficient of friction
>               deriving (Show)

> data Expr = Expr `Mul` Expr
>           | Expr `Div` Expr
>           | Expr `Add` Expr
>           | Expr `Sub` Expr
>           | Sin Expr
>           | Asin Expr
>           | Cos Expr
>           | Acos Expr
>           | Var Property
>           deriving (Show)

> data Equ = Expr `Equ` Expr
>          deriving (Show)

> eqAngle = Var Gamma `Equ` (Var Alpha `Sub` Var Beta)

> eqForcePlane = Var Fp `Equ` (Var F `Mul` Cos (Var Gamma))

> eqForceNormal = Var Fn `Equ` (Var F `Mul` Sin (Var Gamma))

> eqGravitationPlane = Var Gp `Equ` (Var G `Mul` Sin (Var Beta))

> eqGravitationNormal = Var Gn `Equ` (Var G `Mul` Cos (Var Beta))

> eqFriction = Var Ff `Equ` (Var U `Mul` Var N)

> eqNormals = (Var N `Add` Var Fn) `Equ` Var Gn

> eqPlane = ((Var Gp `Sub` Var Ff) `Sub` Var Fp) `Equ`
>           Var Fnet

> eqNewton = (Var A `Mul` Var M) `Equ` Var Fnet

Dessa ekvationer ska gälla i alla fall, dvs alla lösningar till olika uppgifter. Ett sätt att kolla lösningen är då att sätta in alla värden i ekvationerna och se att de stämmer. Detta kan man göra både på papper, men vi gör det här även i datorn.

> type PropVals = Property -> Double

> eval :: Expr -> PropVals -> Double
> eval (e1 `Mul` e2) pv = eval e1 pv * eval e2 pv
> eval (e1 `Div` e2) pv = eval e1 pv / eval e2 pv
> eval (e1 `Add` e2) pv = eval e1 pv + eval e2 pv
> eval (e1 `Sub` e2) pv = eval e1 pv - eval e2 pv
> eval (Sin e) pv = sin $ eval e pv
> eval (Asin e) pv = asin $ eval e pv
> eval (Cos e) pv = cos $ eval e pv
> eval (Acos e) pv = acos $ eval e pv
> eval (Var p) pv = pv p

För att undvika avrundningsfel.

> almostEqual :: Double -> Double -> Bool
> almostEqual d1 d2 = abs (d1-d2) < 0.01

> check :: Equ -> PropVals -> Bool
> check (lhs `Equ` rhs) pv = almostEqual lhs' rhs'
>   where
>     lhs' = eval lhs pv
>     rhs' = eval rhs pv

Ett exempel.

- Lådan väger 70 kg.
- Vinkeln är 30 grader.
- Normal tyngdacceleration på 9,82 m/s^2 gäller.
- Vad blir lådans acceleration?

Vi räknar och inser att `a = g * sin v`. Alltså är accelerationen 4.909999999 m/s^2

Vi testar.

< f Alpha = undefined
< f Beta = 30 * pi / 180
< f Gamma = undefined
< f F = 0
< f Fp = 0
< f Fn = 0
< f G = 9.82
< f Gp = ...

Problmet med detta sättet att modellera är att det finns ekvivalenta sätt att uttrycka egenskaper. T.ex. tyngdkraften i form av bara neråt + vinkel på planet, eller tyngdkraften i två komposanter. Har vi den ena representationen vill vi inte behöva räkna ut den andra manuellet bara för att kontrollräknaren ska fungera.

Hur vet vi vilka som är ekvivalenta? Man använder samma tankesätt som när man ritar kraftdiagram. Efter en komposantuppdelning finns det ju inte plötsligt fler krafter, så man måste ta bort en av representationerna.

Vi gör nu så här att vi i våra samband "stoppar in" de ekvivalenta ekvationerna i andra.

\begin{align}
  f &= N * \mu \\
  N + F * sin (\alpha - \beta)  &= G * cos \beta \\
  G * sin \beta - f - F * cos (\alpha - \beta) &= a * m \\
\end{align}


Nja även detta blir inte bra, för man kan lösa problemet utan att blanda in många av 





Inte ekvationslösare. Man vet redan hur man löser ekvationer, ger ingen fysik-kunskap att göra det automatiskt.

Problemet med en ekvationslösare är att finns väldigt många olika fall. Är ett stort ekvationssystem egentligen. Därför funkar i den "raka" komponeringen av funktioner jag skrivit om.

Tror får nöjga mig med att testa att alla samband stämmer, just för att lösningen kan se så olika ut. Variabler ej inblandade kan vara "don't care", sådana samband skippas kollas. Kan dock bli problematiskt om vinkel från acceleration. Allt går ju via massa. Kanske kan ha att dom egenskaperna får vilket värde som helst om ej specas/underbestämt?



En ide är att göra något som följande. Att som ovan, men mer generellt. Variabler och operationer är strängar. Man länkar samman dem som innan men istället för konstruktorer är det bara en konstruktor med sträng-fält.

< data Prop = Prop String

< evalProp :: (String -> Double) -> Prop -> Double
< evalProp f p = f p

< -- f is user-provided, could look like
< propTable :: String -> Double
< propTable "alpha" = 60
< propTable "beta"  = 30
< propTable "gamma" = 30

< alpha = Prop "alpha"
< beta  = Prop "beta"
< gamma = Prop "gamma"

< data Expr = Expr String [Expr]
<           | Var Prop

< sub = Expr "sub"

< rhsAngles = sub [alpha, beta]

< evalExpr :: (String -> [Double] -> Double) -> (String -> Double) -> Expr -> Double
< evalExpr g f (Expr str exprs) = let exprs' = map (evalExpr f g) exprs
<                                 in g str exprs'

Obs! Kan vara så att flera "properties" är okända. 

Kanske börja specifikt, och sedan generalisera.







Gammalt 1.0
-----------

| Egenskap                  | Beteckning |
|---------------------------|------------|
| Vinkel                    | v          |
| Acceleration längs planet | a          |
| Tyngdacceleration         | g          |
| Friktion                  | u          |
| Normalkraft

Dessa egenskaper förhåller sig till varandra med ekvationer. Exempelvis `a = g * sin v`. I en lutande-plan uppgift kan man bli ombedd att hitta en av dessa egenskaper givet de andra egenskaperna. Har man den ekvationen och får `v` är det enkelt. Tvärtom blir lite lurigare, eftersom man här får omforma ekvationen till `v = sin^-1 (a / g)`.

< data TP = V
<         | A
<         | G

< data Expr = Expr `Mul` Expr
<           | Expr `Div` Expr
<           | Var TP
<           | Sin  Expr
<           | Sin1 Expr

< data Equ = Expr `Equals` Expr

Sambandet i grundform kan då kodas upp i Haskell som

< lhs1 = Var A
< rhs1 = Var G `Mul` Sin (Var V)
< equation1 = lhs1 `Equals` rhs1

Ekvationen `a = g * sin v` och dess omformning `v = sin^-1 (a / g)` är ekvivalenta. Att koda upp båda är därför onödigt.

Uttryck systemet som ett gäng ekvationer. Man matar in data till systemet, och dessa ska klara en quickcheck.

< (===) :: Double -> Double -> Bool
< a === b = abs (a-b) < 0.001

< eval :: Double -> Double -> Expr -> Double
< eval a v (e1 `Mul` e2) = eval a v e1 * eval a v e2
< eval a v (e1 `Div` e2) = eval a v e1 / eval a v e2
< eval a v (Sin  e) = sin (eval a v e)
< eval a v (Sin1 e) = asin (eval a v e)
< eval a v (Var A) = a
< eval a v (Var V) = v
< eval a v (Var G) = 9.82

< check :: Double -> Double -> Equ -> Bool
< check a v (e1 `Equals` e2) = eval a v e1 === eval a v e2