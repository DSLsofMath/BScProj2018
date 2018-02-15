
Typnivå
=======

Vi ska nu behandla enheter på *typnivå*. Vad är typnivå? När man brukar programmera (i Haskell) gör man operationer (till exempel `+`) på värden (till exempel `1` och `2`). Detta är på *värdenivå*. Nu ska vi göra samma sak fast på typnivå, det vill säga, göra operationer på typer.

Varför ha enheter på typnivå? För då går det att se redan vid kompileringstillfället om det man skriver är riktigt.

Som tidigare nämnt kommer implementationen här vara snarlik den på värdenivå.

För att klara av det vi ska göra nu krävs en drös GHC-extensions. TODO: Förklara vad de behövs till kanske.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE TypeOperators #-}
> 
> module Units.TypeLevel
> ( Unit(..)
> , Mul
> , Div
> , Length
> , Time
> , Mass
> , Temperature
> , Substance
> , One
> )
> where

Vi kommer behöva hantera heltal på typnivå. Istället för att implementera det själva importerar vi hela maskinerit så att vi kan fokusera på fysik-biten.

> import Numeric.NumType.DK.Integers

Vi gör en *sort* för enheter igen. På värdenivå gjorde vi en *typ* med *värden*. Nu gör vi en *sort* med *typer*. Det är exakt samma betydelse, men flyttat "ett steg upp".

> data Unit = Unit TypeInt -- Length
>                  TypeInt -- Time
>                  TypeInt -- Mass
>                  TypeInt -- Temperature
>                  TypeInt -- Amount of substance

Men `data Unit = ...` ser ju ut som en helt vanlig datatyp? Det är riktigt. Men med `DataKinds` skapar detta en normal datatyp precis som vanligt **och** en *sort*. En mindre förvirrande syntax hade kanske varit `kind Unit = ...`.

Med hjälp av `Unit`-sorten kan vi tvinga vissa typer i funktioner att vara av den sorten.

Detta kan låta förvirrande, men poängen med detta kommer klarna allt efter hand. Låt oss visa några exempel*typer* ur `Unit`-sorten.

> type Length      = 'Unit Pos1 Zero Zero Zero Zero
> type Time        = 'Unit Zero Pos1 Zero Zero Zero
> type Mass        = 'Unit Zero Zero Pos1 Zero Zero
> type Temperature = 'Unit Zero Zero Zero Pos1 Zero
> type Substance   = 'Unit Zero Zero Zero Zero Pos1
> 
> type Velocity     = 'Unit Pos1 Neg1 Zero Zero Zero
> type Acceleration = 'Unit Pos1 Neg2 Zero Zero Zero
> 
> type One = 'Unit Zero Zero Zero Zero Zero

`'Unit` används för att skilja mellan *typen* `Unit` och *typkonstruktorn* `Unit`. `'Unit` syftar på typkonstruktorn. Båda skapas parallellt i Haskell.

`Pos1`, `Neg1` och så vidare motsvarar `1` och `-1` i det importerade paketet, som behandlar tal på typnivå.

Vi gör nu multiplikation och division på typnivå. Efter en sådan operation bildas en ny enhet. Och hur detta går till vet vi sedan tidigare. För att översätta till Haskell-språk: "efter en sådan operation bildas en ny *typ*". Hur implementerar man det? Med hjälp av `type family`, som enklast kan ses som en funtion fast på typnivå.

> type family Mul (u1 :: Unit) (u2 :: Unit) where
>   Mul ('Unit l1 t1 m1 k1 s1) ('Unit l2 t2 m2 k2 s2) =
>     'Unit (l1+l2) (t1+t2) (m1+m2) (k1+k2) (s1+s2)

- `type family` anger att det är en funktion på typnivå.
- `Mul` är namnet.
- `u1 :: Unit` utläses "typen `u1` har sorten `Unit`.

Division är snarlik.

> type family Div (u1 :: Unit) (u2 :: Unit) where
>   Div ('Unit l1 t1 m1 k1 s1) ('Unit l2 t2 m2 k2 s2) =
>     'Unit (l1-l2) (t1-t2) (m1-m2) (k1-k2) (s1-s2)

Låt oss skapa några exempel*typer* för enheter med hjälp av multiplikation och division.

> type Velocity' = Length `Div` Time
> type Area      = Length `Mul` Length
> type Force     = Mass   `Mul` Length
> type Impulse   = Force  `Mul` Time

Inte så spännande än så länge. Men vänta tills vi skapar en datatyp för storheter. Då blir det tydligare med styrkorna med att ha enheter på typnivå utöver värdenivå.