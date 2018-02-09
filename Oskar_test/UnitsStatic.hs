
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (Maybe)

-- Naturliga tal som typ
data TNat = Zero | Succ TNat

--add :: TNat -> TNat -> TNat
--add Zero     n = n
--add (Succ n) m = add n (Succ m)

-- Funktion fast för typer
-- n :: TNat "n har sorten TNat"
type family TAdd (n::TNat) (m::TNat) where
  TAdd 'Zero n = n
  TAdd ('Succ n) m = TAdd n ('Succ m)


data BaseUnit = Length
              | Time
              | Mass
              deriving (Eq, Show)

data Unit = Single BaseUnit
          | Cons BaseUnit Unit
          deriving (Eq, Show)



type family Mul (n :: Unit) (m :: Unit) where
  Mul (Single bu1) (Single bu2) = Cons bu1 (Single bu2)
  Mul (Single bu)  u            = Cons bu  u
  Mul u            (Single bu)  = Cons bu  u
  
type X = Single Length
type T = Single Time

type V = Mul X T
type V' = Cons Length (Single Time)

type family Div (n :: Unit) (m :: Unit) where
  Div (Cons Length u) (Single Length) = u
  Div (Cons Time   u) (Single Time)   = u
  Div (Cons Mass   u) (Single Mass)   = u
  Div (Cons bu1    u) (Single bu2)    = Cons bu1 (Div u (Single bu2))
  Div (Cons Length u1) (Cons Length u2) = Div u1 u2
  -- Detta är bara tillfälligt
  Div (Single bu1) (Single bu2) = Single bu1

type Volume = Mul (Mul (Single Length) (Single Length)) (Single Length)
type Area = Mul (Single Length) (Single Length)

type Len = Div Volume Area
type Area' = Div Volume (Single Length)



--mult :: (Double, u1) -> (Double, u2) -> (Double, Mul u1 u2)
--mult = undefined

--appned :: u1 -> u2 -> (Mul u1 u2)
--appned = undefined

data Quantity (u :: Unit) (a :: *) where
  Kvant :: a -> Quantity u a

multi :: (Num a) => Quantity u1 a -> Quantity u2 a -> Quantity (Mul u1 u2) a
multi (Kvant x) (Kvant y) = Kvant (x*y)

-- Ett papper har bredd 5 meter
-- och längd 3 meter
-- Beräkna ytan

bredd :: Quantity Len Double
bredd = Kvant 5

langd :: Quantity Len Double
langd = Kvant 3

ytan :: Quantity Area Double
ytan = multi bredd langd


addi :: (Num a) => Quantity u a -> Quantity u a -> Quantity u a
addi (Kvant x) (Kvant y) = Kvant $ x+y


omkrets :: Quantity Len Double
omkrets = addi bredd bredd `addi` addi langd langd

-- Nedanstående kompilerar inte
--skum = addi bredd ytan
