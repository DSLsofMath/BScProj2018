
{-# LANGUAGE TypeOperators #-}

module Combination.Combination
    (
    ) where

import Vector.Vector
import Calculus.FunExpr
import Calculus.DifferentialCalc
import Dimensions.Quantity
import Dimensions.TypeLevel
import Prelude hiding (length)

-- Calculus i Dimensions
------------------------

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









































