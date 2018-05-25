
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Combination.Combination
    (
    ) where

import Vector.Vector2
--import Calculus.FunExpr
--import Calculus.DifferentialCalc
--import Calculus.IntegralCalc
import Dimensions.Quantity2
import Dimensions.TypeLevel
import qualified Dimensions.ValueLevel as V
import Prelude hiding (length)

-- Calculus i Dimensions
------------------------

{-

instance Fractional FunExpr where
  fromRational = Const . fromRational
  (/)          = (:/)

instance Floating FunExpr where
  pi = Const pi
  exp = (Exp :.)
  log = (Log :.)
  sin = (Sin :.)
  cos = (Cos :.)
  asin = (Asin :.)
  acos = (Acos :.)
  atan = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

-- Ett flygplan med position 5+2t
t1 :: Quantity Length FunExpr
t1 = (Const 5 :+ Const 2 :* Id) # length

-- eller kortare
type QC d = Quantity d FunExpr

t2 :: QC Length
t2 = t1

-- Ett flygplan i turbulens
t3 :: QC Length
t3 = 5 + sin Id # length

-- Ett flygplan som står stilla
t4 :: QC Length
t4 = 8 # length

type Area = Length `Mul` Length

-- Någon slags multiplikation
t5 :: QC Area
t5 = t1 *# t3

t6 :: QC Area
t6 = fmap simplify t5

-- Någon slags division
t7 :: QC Length
t7 = t6 /# t4

-- Går ej, vilket är bra
-- t8 :: QC Area
-- t8 = t6 /# t4

-- Stöd för derivering. Hela QC och dessa
-- bör vara dola för utomstående användare.

-- fmap på Quantity, men inte QC!

differentiateWRTtime :: QC d -> QC (d `Div` Time)
differentiateWRTtime qc = fmap simplify $ fmap derive qc /# time

integrateWRTtime :: QC d -> QC (d `Mul` Time)
integrateWRTtime qc = fmap simplify $ fmap integrate qc *# time

-}


--------------------
-- Calc i Vec i Quan
--------------------

-----------------------
-- Instansiering

-- En vektor kan adderas

-- (Kan alla göras samtidigt?)

instance (Addable v v v) => Addable (Vector2 v) (Vector2 v) (Vector2 v) where
  doAdd = vzipWith (doAdd)
instance (Addable v v v) => Addable (Vector3 v) (Vector3 v) (Vector3 v) where
  doAdd = vzipWith (doAdd)

-- En vektor kan multipliceras på flera sätt

-- Skalning (från vänster)
instance (Num v) => Multiplicable v (Vector2 v) (Vector2 v) where
  doMult = scale
instance (Num v) => Multiplicable v (Vector3 v) (Vector3 v) where
  doMult = scale

-- Kryssprdoukt
instance (Num v) => Multiplicable (Vector3 v) (Vector3 v) (Vector3 v) where
  doMult = crossProd

-- Skalärprodukt
instance (Num v) => Multiplicable (Vector2 v) (Vector2 v) v where
  doMult = dotProd
instance (Num v) => Multiplicable (Vector3 v) (Vector3 v) v where
  doMult = dotProd

-- En vektor kan "skapas"

instance (Creatable v) => Creatable (Vector2 v) where
  anyVal = V2 anyVal anyVal
instance (Creatable v) => Creatable (Vector3 v) where
  anyVal = V3 anyVal anyVal anyVal

------------------------------------
-- Användning

-- Ej dimensionssäkra
v1 :: Vector3 Double
v1 = V3 3 2 3
v2 :: Vector3 Double
v2 = V3 1 2 5
v3 :: Vector3 Double
v3 = V3 7 8 2

-- Dimensionsäkra
v1d :: Quantity Length (Vector3 Double)
v1d = v1 ## length
v2d :: Quantity Mass (Vector3 Double)
v2d = v2 ## mass
v3d :: Quantity Time (Vector3 Double)
v3d = v3 ## time

-- t1 kräver typsignatur, antagligen för den här MultiParam...
-- så att ska veta vilken instans

-- Addition
t1 :: Quantity Length (Vector3 Double)
t1 = v1d +# v1d

-- Kryssprodukt
t2 :: Quantity (Length `Mul` Mass) (Vector3 Double)
t2 = v1d *# v2d

-- Skalning
t3 :: Quantity (Length `Mul` Mass) (Vector3 Double)
t3 = s *# v2d
  where
    s :: Quantity Length Double
    s = 3.0 ## length

-- Skalärprodukt
t4 :: Quantity (Time `Mul` Length) Double
t4 = v3d *# v1d























































