
Storheter
=========

Vi ska nu skapa en datatyp för storheter och kombinera enheter på typnivå och värdesnivå. Precis som tidigare krävs en drös GHC-extensions.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE TypeOperators #-}
> 
> module Units.Quantity
> (
> )
> where
> 
> import qualified Units.ValueLevel as V
> import           Units.TypeLevel  as T
> import           Prelude          as P hiding (length, div)

Först skapar vi själva datatypen.

> data Quantity (u :: T.Unit) (r :: *) where
>   Quantity :: r -> V.Unit -> Quantity u r

- `data Quantity` skapar en *typ*.
- `u` en *typ* och `T.Unit` en *sort*. I slutändan kommer vår datatyp ha en typparameter där typen måste ha sorten `T.Unit`.
- `r :: *` betyder att `r`, som i **r**eellt, har sorten `*` som är sorten av typer som kan ha värden.
- `Quantity` på raden nedanför är en värdekonstruktor.
- Värdekonstruktorn har två *värde*-parametrar som ska ha vissa *typer*.
  - `r` är en typ som representerar ett tal (t.ex. `Double` eller `Int`).
  - `V.Unit` är storhetens enhet på värdesnivå.
- `Quantity` på övre raden är namnet på en typ (snarare typkonstruktor eftersom den har de två parametrarna `u` och `r`) medan `Quantity` på den nedre raden är namnet på ett värde (värdekonstruktor). Samma namn men olika saker. Det är möjligt att göra så här, precis som definitionen nedan är möjlig med samma namn på olika saker av de två sidorna av lika-med-tecknet.

Motsvarande datatyp utan enheter på typnivå skulle se ut som

< data Quantity r = Quantity V.Unit r

`u` i den tidigare definitionen ska inte förväxlas med `V.Unit` i den senare. `u` är namnet på en obunden typ, som ska ha sorten `T.Unit` medan `V.Unit` anger att här ska ett värde vara av typen `V.Unit`. Det står `u` istället för `T.Unit` av syntaktiska skäl, men man kan tänka att det ska stå `T.Unit` istället för att enklare förstå.

Vi ska implementera alla räknesätt för `Quantity`, men för att få ett smakprov visar vi här addition och multiplikation samt några exempel på värden av typ `Quantity`.

> quantityAdd :: (Num v) => Quantity u v -> 
>                           Quantity u v ->
>                           Quantity u v
> quantityAdd (Quantity v1 u) (Quantity v2 _) = Quantity (v1+v2) u

Typen tolkas så här: som indata tas två värden av typen `Quantity u v` där `u` är enheten som typ. Utdata blir en `Quantity u v` också.

Typerna på funktionen tvingar indata att ha samma enheter. Därför kollar spelar typen på värdenivå på ett av argumenten ingen roll, för de kommer vara samma. (Man kan skapa värden där enheterna på värdenivån inte är samma som på typnivån. Detta återkommer vi till.)

Multiplikation blir:

> quantityMul :: (Num v) => Quantity u1 v -> 
>                           Quantity u2 v ->             
>                           Quantity (Mul u1 u2) v
> quantityMul (Quantity v1 u1) (Quantity v2 u2) = 
>   Quantity (v1*v2) (V.mul u1 u2)

Typen tolkas så här: som indata tas två värden av typen `Quantity ux v` där `ux` är två typer som representerar enheter. Som utdata får man en `Quantity` med enheten av typen som är produkten av de två enhterna in. Högst naturligt alltså!

Några exempelvärden nu.

> width :: Quantity T.Length Double
> width = Quantity 0.5 V.length
> 
> height :: Quantity T.Length Double
> height = Quantity 0.3 V.length
> 
> type Area = Mul T.Length T.Length

Nedanstående visar att vid en multiplikation så påverkas typerna, som sig bör. Det är inte bara något på värdesnivån som ändras utan även typerna.

> area :: Quantity Area Double
> area = quantityMul width height

Att typerna motsvarar enheter användas nedan för att vid kompileringstid avgöra om en operation får göras.

< -- Komilerar inte ens
< skum = quantityAdd width area

Om man haft enheterna enbart på värdesnivå hade man  upptäckt felet först vid körning.

Pretty-printer
--------------

Nu ska vi göra en pretty-printer för en storhet. Det stora jobbet är redan avklarat i enheter på värdenivå.

> showQuantity :: (Show v) => Quantity u v -> String
> showQuantity (Quantity v u) = show v ++ " " ++ show u
> 
> instance (Show v) => Show (Quantity u v) where
>   show = showQuantity

Jämförelser
-----------

Det är användbart att jämföra storheter. Man vill kanske veta vilken av två energimängder som är störst. Men vad är störst av `1 J` och `1 m`? Det är ingen meningsfull jämförelse eftersom enheterna inte är samma. För att skydda oss frånt sådant har vi enheterna på typnivå.

> quantityEq :: (Eq v) => Quantity u v -> Quantity u v -> Bool
> quantityEq (Quantity v1 _) (Quantity v2 _) = v1 == v2

Vi kan göra `Quantity` en `Eq`-instans.

> instance (Eq v) => Eq (Quantity u v) where
>   (==) = quantityEq

Vi implementerar också `Ord`.

> quantityCompare :: (Ord v) => Quantity u v -> 
>                               Quantity u v -> Ordering
> quantityCompare (Quantity v1 _) (Quantity v2 _) =
>   compare v1 v2
> 
> instance (Ord v) => Ord (Quantity u v) where
>   compare = quantityCompare

Färdiga storheter
-----------------

Vill man skapa en variabel som representerar en viss sträcka (till exempel 5 meter) gör man som nedanstående:

< distance :: Quantity T.Length Double
< distance = Quantity 5 V.length

Att skriva så varje gång blir klumpigt. Dessutom kan man göra "dumma" saker som

< distance :: Quantity T.Length Double
< distance = Quantity 5 V.time

Här har man olika enheter på värdenivå och typnivå.

För att lösa dessa problem kommer vi introducera lite syntaktiskt socker. Först funktioner som skapar de 5 grundläggande storheterna samt skalären (vi ska göra samma sak för sammansatta storheter senare).

> length :: (Num v) => Quantity Length v
> length = Quantity 0 V.length
> 
> time :: (Num v) => Quantity Time v
> time = Quantity 0 V.time
> 
> mass :: (Num v) => Quantity Mass v
> mass = Quantity 0 V.mass
> 
> temperature :: (Num v) => Quantity Temperature v
> temperature = Quantity 0 V.temperature
> 
> substance :: (Num v) => Quantity Substance v
> substance = Quantity 0 V.substance
> 
> one :: (Num v) => Quantity One v
> one = Quantity 0 V.one

Och nu sockret.

> (#) :: (Num v) => v -> Quantity u v -> Quantity u v
> v # (Quantity _ u) = Quantity v u

Den funktion är tänkt att användas som följande:

< ghci> let myDistance = 5 # length
< ghci> :t myDistance
< t :: Num v => Quantity Length v

För att skapa en storhet med ett mätetal, i detta fallet `5`, skriver man som ovan, så får man en storhet-variabel direkt med rätt enhet på både värdenivå och typnivå.

`length`, `time` med mera är bara dummy-variabler med rätt enhet (på både värdenivå och typnivå) för att kunna skapa storhets-variabler enklare. Vilket värde som helst med rätt enhet på bägge nivåerna kan användas som högersidans argument.

< ghci> let otherPersonsDistance = 10 # length
< ghci> let myDistance = 5 # otherPersonsDistance
< ghci> :t myDistance
< t :: Num v => Quantity Length v

Precis samma sak.

Räkneoperationer på kvantiteter
-------------------------------

Nu ska vi implementera räkneoperationer för storheter. I stora drag handlar det om att skapa funktioner med rätt enheter på typnivå.

> (+#) :: (Num v) => Quantity u v -> Quantity u v -> 
>                    Quantity u v
> (+#) = quantityAdd
> 
> (-#) :: (Num v) => Quantity u v -> Quantity u v -> 
>                    Quantity u v
> (Quantity v1 u) -# (Quantity v2 _) = Quantity (v1-v2) u
> 
> (*#) :: (Num v) => Quantity u1 v -> Quantity u2 v ->
>                    Quantity (u1 `Mul` u2) v
> (*#) = quantityMul
> 
> (/#) :: (Fractional  v) => Quantity u1 v -> 
>                            Quantity u2 v ->
>                            Quantity (u1 `Div` u2) v
> (Quantity v1 u1) /# (Quantity v2 u2) =
>   Quantity (v1 / v2) (u1 `V.div` u2)

Vid alla fyra räknesätt på storheter gör man räknesättet på mätetalet och, vid fallet multiplikation och divsion, på enheten för sig. Addition och subtraktion däremot kräver att de två in-enheterna är samma.

Hur går det till att göra operationer som `sin` på en storhet med en *eventuell* enhet? Svaret är att storheten måste vara enhetslös för att det ska fungera, och med enheten händer då ingenting. Vi kräver därför att storheterna in är enhetslösa för nedanstående funktioner.

< sinq :: (Floating v) => Quantity One v -> Quantity One v
< sinq (Quantity v ul) = Quantity (sin v) ul
< 
< cosq :: (Floating v) => Quantity One v -> Quantity One v
< cosq (Quantity v ul) = Quantity (cos v) ul

Vi inser snabbt ett mönster, så låt oss generalisera lite.

> qmap :: (a -> b) -> Quantity One a -> Quantity One b
> qmap f (Quantity v ul) = Quantity (f v) ul
> 
> type BinaryScalar v = Quantity One v -> Quantity One v
> 
> sinq, cosq, asinq, acosq, atanq, expq, logq :: (Floating v) =>
>   BinaryScalar v
> sinq  = qmap (sin)
> cosq = qmap (cos)
> asinq = qmap (asin)
> acosq = qmap (acos)
> atanq = qmap (atan)
> expq  = qmap (exp)
> logq  = qmap (log)

En fråga man kan ställa sig efter detta är "Varför inte göra Quantity en instans av `Num`, `Fractional`, `Floating` och `Functor`?" Svaret ligger i att dessa typklasser har funktioner av nedanstående typ

< (*) :: (Num a) => a -> a -> a

och det är inte förenligt med `Quantity` eftersom multiplikation med `Quantity` har följande typ

< (*#) :: (Num v) => Quantity u1 v -> Quantity u2 v ->
<                   Quantity (u1 `Mul` u2) v

Indatan får här faktiskt vara av *olika* typer, och utdatan blir en typ som beror på indatans typer. Däremot är *sorterna* på de tre argumentens typer samma, nämligen `Quantity`. Vi får helt enkelt leva med att inte kunna göra `Quantity` en `Num`-instans.

Operationerna när enbart skalärer är inblandade har däremot typer förenliga med `Num`. Därför hade det varit möjligt att göra `Quantity One` en `Num`-instans.

Tillbaka till färdiga storheter
-------------------------------

Som antyddes fanns det ett tvåfaldigt syfte till att skapa "färdiga storheter". Det ena skälet var att göra det enklare att skapa sina egna storhets-variabler. Det andra skälet är att vi vill ha som invariant att enheten på värdenivå och typnivå är samma. Genom att bara tillåta "användaren" använda dessa färdiga storheter kan vi tvinga fram denna invarient.

Låt oss skapa resterande färdiga storheter.

> velocity = length /# time
> acceleration = velocity /# time
> force = mass *# acceleration
> impulse = force *# time
> energy = force *# length

TODO: Dessa har alla Double som värdetyp. Hur förhindra det? Explicita typsignaturer löser det, men man vill inte att "användaren" ska behöva och få göra det för att behålla den tidigare nämnda invarianten.

Är detta alla tänkabara storheter inom *Fysik för ingenjörer*? Självklart inte. Precis som vi skapade dessa storheter, kan man skapa sina egna vid behov.

< specificHeatCapacity = energy /# (mass *# temperature)

Dessutom "uppstår" enheter vid behov.

< ghci> let heatConsumed = 5000 # energy
< ghci> let massOfLiquid = 9.3 # mass
< ghci> let temperatureDifference = 25 # temperature
< ghci> heatConsumed /# (massOfLiquid *# temperatureDifference)
< 21.50537634408602 m^2/(K*s^2)