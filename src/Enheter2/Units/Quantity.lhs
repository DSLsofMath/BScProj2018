
 ## Kvanitiet

Vi ska nu skapa en datatyp för kvantitet och kombinera enheter på typnivå och värdesnivå.

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
> import Units.ValueLevel as V
> import Units.TypeLevel as T
> import Prelude hiding (length, div)

Först skapas datatypen.

> data Quantity (u :: T.Unit) (r :: *) where
>   Quantity :: r -> V.Unit -> Quantity u r

- `data Quantity` skapar en *typ*.
- `u` en *typ* och `T.Unit` en *sort*. I slutändan kommer vår datatyp ha en typparameter där typen måste ha sorten `T.Unit`.
- `r :: *` betyder att `r`, som i **r**eellt, har sorten `*` som är sorten av typer som kan ha värden.
- `Quantity` på raden nedanför är en datakonstruktor.
- Datakonstruktorn har två *värde*-parametrar.
  - `r` är en typ som representerar ett tal (t.ex. `Double` eller `Int`).
  - `V.Unit` är kvantitetens enhet på värdesnivå.
- `Quantity` på övre raden är namnet på en typ (snarare typkonstruktor eftersom den har de två parametrarna `u` och `r`) medan `Quantity` på den nedre raden är namnet på ett värde (värdekonstruktor). Samma namn men olika saker. Det är möjligt att göra så här, precis som definitionen nedan är möjlig med samma namn på olika saker av de två sidorna av lika-med-tecknet.

Motsvarande datatyp utan enheter på typnivå skulle se som

< data Quantity r = Quantity V.Unit r

`u` i den tidigare definitionen ska inte förväxlas med `V.Unit` i den senare. `u` är namnet på en obunden typ, som ska ha sorten `T.Unit` medan `V.Unit` anger att här ska ett värde vara av typen `V.Unit`. Det står `u` istället för `T.Unit` av syntaktiska skäl, men man kan tänka att det ska stå `T.Unit` istället för att enklare förstå.

Vi ska implementera alla räknesätt för `Quantity`, men för att få ett smakrpov visar vi här multiplikation samt några exempelvärden på värden av typ `Quantity`.

> quantityMult :: (Num v) => Quantity u1 v -> Quantity u2 v -> Quantity (Mul u1 u2) v
> quantityMult (Quantity v1 u1) (Quantity v2 u2) = Quantity v1