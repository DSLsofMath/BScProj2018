
Haskell klagar om man har # som första tecken...
 ## Typnivå

Vi ska nu behandla enheter på *typnivå*. Vad är typnivå? Vanligtvis när man progammerar (i Haskell) befinner man sig på *värdenivå*. T.ex. adderar man värdena `1+2`. Värdena måste ha samma *typ* för att de ska kunna adderas, i detta fallet har de kanske `Int` som typ. Typnivå är i princip samma sak, men som det låter, en nivå upp vid typerna. Man gör operationer på typer på samma sätt man gör operationer på värden.

Varför ha enheter på typnivå? För då går det att se redan vid kompileringstid om det man skriver är riktigt.

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
> )
> where

Vi kommer behöva hantera heltal på typnivå. Istället för att implementera det själva importerar vi hela maskinerit så att vi kan fokusera på fysik-biten.

> import Numeric.NumType.DK.Integers

Längd, tid, massa.

> data Unit = Unit TypeInt
>                  TypeInt
>                  TypeInt

Multipliceras två enheter bildas en ny enhet. Och hur detta går till vet vi sedan tidigare. För att översätta till Haskell-språk: "multipliceras två typer bildas en ny typ". Hur implementerar man det? Med hjälp av `type family`, som enklast kan ses som en funtion fast på typnivå.

> type family Mul (u1 :: Unit) (u2 :: Unit) where
>   Mul ('Unit l1 t1 m1) ('Unit l2 t2 m2) = 'Unit (l1+l2) (t1+t2) (m1+m2)

Division implementeras snarlikt.

> type family Div (u1 :: Unit) (u2 :: Unit) where
>   Div ('Unit l1 t1 m1) ('Unit l2 t2 m2) = 'Unit (l1-l2) (t1-t2) (m1-m2)

Låt oss skapa några exempel*typer* för några enheter.

> type Length = 'Unit Pos1 Zero Zero
> type Time   = 'Unit Zero Pos1 Zero
> type Mass   = 'Unit Zero Zero Pos1
> 
> type Area   = Mul Length Length
> type Area'  = 'Unit Pos2 Zero Zero
> 
> type Velocity  = 'Unit Pos1 Neg1 Zero
> type Velocity' = Div Length Time

Inte så spännande än så länge. Men vänta tills vi skapar en datatyp för kvaniteter (innehåller värde och enhet). Då blir det tydligare med styrkorna med att ha enheter på typnivå.