
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
> import Units.TypeLevel as T
> import Prelude as P hiding (length, div)

Först skapar vi själva datatypen.

> data Quantity (u :: T.Unit) (r :: *) where
>   Quantity :: r -> V.Unit -> Quantity u r

- `data Quantity` skapar en *typ*.
- `u` en *typ* och `T.Unit` en *sort*. I slutändan kommer vår datatyp ha en typparameter där typen måste ha sorten `T.Unit`.
- `r :: *` betyder att `r`, som i **r**eellt, har sorten `*` som är sorten av typer som kan ha värden.
- `Quantity` på raden nedanför är en datakonstruktor.
- Datakonstruktorn har två *värde*-parametrar.
  - `r` är en typ som representerar ett tal (t.ex. `Double` eller `Int`).
  - `V.Unit` är storhetens enhet på värdesnivå.
- `Quantity` på övre raden är namnet på en typ (snarare typkonstruktor eftersom den har de två parametrarna `u` och `r`) medan `Quantity` på den nedre raden är namnet på ett värde (värdekonstruktor). Samma namn men olika saker. Det är möjligt att göra så här, precis som definitionen nedan är möjlig med samma namn på olika saker av de två sidorna av lika-med-tecknet.

Motsvarande datatyp utan enheter på typnivå skulle se ut som

< data Quantity r = Quantity V.Unit r

`u` i den tidigare definitionen ska inte förväxlas med `V.Unit` i den senare. `u` är namnet på en obunden typ, som ska ha sorten `T.Unit` medan `V.Unit` anger att här ska ett värde vara av typen `V.Unit`. Det står `u` istället för `T.Unit` av syntaktiska skäl, men man kan tänka att det ska stå `T.Unit` istället för att enklare förstå.

Vi ska implementera alla räknesätt för `Quantity`, men för att få ett smakprov visar vi här addition och multiplikation samt några exempelvärden på värden av typ `Quantity`.

> quantityAdd :: (Num v) => Quantity u v -> 
>                           Quantity u v ->
>                           Quantity u v
> quantityAdd (Quantity v1 u) (Quantity v2 _) = Quantity (v1 P.+ v2) u

Typen tolkas så här: som indata tas två värden av typen `Quantity u v` där `u` är enheten som typ. Utdata blir en `Quantity u v` också.

Typerna på funktionen tvingar indata att ha samma enheter. Därför kollar spelar typen på värdenivå på ett av argumenten ingen roll, för de kommer vara samma. (Man kan skapa värden där enheterna på värdenivån inte är samma som på typnivån. Detta återkommer vi till.)

Multiplikation blir:

> quantityMul :: (Num v) => Quantity u1 v -> 
>                           Quantity u2 v ->             
>                           Quantity (Mul u1 u2) v
> quantityMul (Quantity v1 u1) (Quantity v2 u2) = 
>   Quantity (v1 P.* v2) (V.mul u1 u2)

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

Räkneoperationer på kvantiteter
-------------------------------

Nu ska vi implementera räkneoperationer för storheter. I stora drag handlar det om att skapa funktioner med rätt enheter på typnivå.

TODO: Varför inte Num instanser?

> (+) :: (Num v) => Quantity u v -> Quantity u v -> 
>                   Quantity u v
> (+) = quantityAdd
> 
> (-) :: (Num v) => Quantity u v -> Quantity u v -> 
>                   Quantity u v
> (Quantity v1 u) - (Quantity v2 _) = Quantity (v1 P.- v2) u
> 
> (*) :: (Num v) => Quantity u1 v -> Quantity u2 v ->
>                   Quantity (u1 `Mul` u2) v
> (*) = quantityMul
> 
> (/) :: (Fractional  v) => Quantity u1 v -> 
>                           Quantity u2 v ->
>                           Quantity (u1 `Div` u2) v
> (Quantity v1 u1) / (Quantity v2 u2) =
>   Quantity (v1 P./ v2) (u1 `V.div` u2)

Vid alla fyra räknesätt på storheter gör man räknesättet på mätetalet och, vid fallet multiplikation och divsion, på enheten för sig. Addition och subtraktion däremot kräver att de två in-enheterna är samma.

Hur går det till att göra operationer som `sin` på en storhet med en *eventuell* enhet? Svaret är att storheten måste vara enhetslös för att det ska fungera, och med enheten händer då ingenting. Vi introducerar därför den enhetslösa enheten, eller en skalär:

< type Scalar = 'Unit Zero Zero Zero Zero Zero







15:45







Färdiga storheter
-----------------

Vill man skapa en variabel som representerar en viss sträcka (till exempel 5 meter) gör man som nedanstående:

< distance :: Quantity T.Length Double
< distance = Quantity 5 V.length

Att skriva så varje gång blir klumpigt. Dessutom kan man göra "dumma" saker som

< distance :: Quantity T.Length Double
< distance = Quantity 5 V.time

Här har man olika enheter på värdenivå och typnivå.

För att lösa dessa problem kommer vi introducera lite syntaktiskt socker. Först funktioner som skapar de 5 grundläggande storheterna (vi ska göra samma sak för sammansatta storheter senare).

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

Och nu sockret.

> (#) :: (Num v) => v -> Quantity u v -> Quantity u v
> v # (Quantity _ u) = Quantity v u

Den funktion är tänkt att användas som följande:

< ghci> let myDistance = 5 # length
< ghci> :t myDistance
< t :: Num v => Quantity Length v

För att skapa en storhet med ett mätetal, i detta fallet `5`, skriver man som ovan, så får man en storhet-variabel direkt med rätt enhet på både värdenivå och typnivå.


