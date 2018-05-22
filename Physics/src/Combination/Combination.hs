
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
type CQ d = Quantity d FunExpr

t2 :: CQ Length
t2 = t1

-- Ett flygplan i turbulens
t3 :: CQ Length
t3 = 5 + sin Id # length

-- Ett flygplan som står stilla
t4 :: CQ Length
t4 = 8 # length

type Area = Length `Mul` Length

-- Någon slags multiplikation
t5 :: CQ Area
t5 = t1 *# t3

t6 :: CQ Area
t6 = fmap simplify t5

-- Någon slags division
t7 :: CQ Length
t7 = t6 /# t4

-- Går ej, vilket är bra
-- t8 :: CQ Area
-- t8 = t6 /# t4

-- Stöd för derivering. Hela CQ och dessa
-- bör vara dola för utomstående användare.
-- Inuti är "otypat". Wrap och unwrap borde vara "protected" och inte "public", Java analogi.

differentiateWRTtime :: CQ d -> CQ (d `Div` Time)
differentiateWRTtime cq = newWithNewQuantity
  where
    originalWithQuantity = cq
    newWithQuantity      = fmap derive cq
    newWithoutQuantity   = unwrap newWithQuantity
    newWithNewQuantity   = wrap newWithoutQuantity (originalWithQuantity /# time)








































