
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

-- Vi kommer behöva hantera siffror på typnivå. Istället för att implementerra det själv så importerar vi maskinerit.

import Prelude as V
import Numeric.NumType.DK.Integers as T

-- En enhet är en produkt av olika "grundenheter" med exponenter.
data TUnit = TUnit TypeInt -- Length
                   TypeInt -- Time
                   TypeInt -- Mass

data VUnit = VUnit Integer -- Length
                   Integer -- Time
                   Integer -- Mass

-- Funktion på typ-nivå. Istället för värden av en viss
-- typ, så tar den typer av en viss sort
type family TMul (u1 :: TUnit) (u2 :: TUnit) where
  TMul ('TUnit l1 t1 m1) ('TUnit l2 t2 m2) = 'TUnit (l1+l2) (t1+t2) (m1+m2)

type family TDiv (u1 :: TUnit) (u2 :: TUnit) where
  TDiv ('TUnit l1 t1 m1) ('TUnit l2 t2 m2) = 'TUnit (l1-l2) (t1-t2) (m1-m2)

type TLength = 'TUnit Pos1 Zero Zero
type TTime   = 'TUnit Zero Pos1 Zero
type TMass   = 'TUnit Zero Zero Pos1

type TArea   = TMul TLength TLength
type TArea'  = 'TUnit Pos2 Zero Zero

type TVelocity  = 'TUnit Pos1 Neg1 Zero
type TVelocity' = TDiv TLength TTime


-- Här utläses "::" som "har sort"
data TQuantity (u :: TUnit) (a :: *) where
  -- Här är det däremote "har typ"
  VQuantity :: a -> VUnit -> TQuantity u a

-- Jämför med
-- data Quantity a = Quantity a TUnit
-- Där finns inget sätt att skilja mellan olika
-- enheter på typ nivå, även om enehterna följer
-- med på värdesnivå

vLength, vTime, vMass, vArea, vVelocity :: VUnit
vLength = VUnit 1 0 0
vTime   = VUnit 0 1 0
vMass   = VUnit 0 0 1
vArea   = VUnit 2 0 0
vVelocity = VUnit 1 (-1) 0

quantityMult :: (Num v) => TQuantity u1 v -> TQuantity u2 v -> TQuantity (TMul u1 u2) v
quantityMult (VQuantity v1 u1) (VQuantity v2 u2) = VQuantity v1

-- Exempel. En datorskärm är 0,3 m bred och 0,2 m hög
width :: TQuantity TLength Double
width = VQuantity 0.3 vLength

height :: TQuantity TLength Double
height = VQuantity 0.2 vLength

--area :: TQuantity TArea Double
--area = 














