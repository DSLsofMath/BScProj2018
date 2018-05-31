
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Combination.Calculus where

import Calculus.FunExpr
import Calculus.DifferentialCalc
import Calculus.IntegralCalc

import Combination.Quantity
import Dimensions.TypeLevel

import Combination.Vector

import Prelude hiding (length)

-- FunExpr är redan Num-instans, så borde inte behöva göra detta,
-- men pga okända anledningar funkar det ej

instance Addable FunExpr FunExpr FunExpr where
  doAdd = (+)

instance Subable FunExpr FunExpr FunExpr where
  doSub = (-)

instance Multiplicable FunExpr FunExpr FunExpr where
  doMul = (*)

instance Divisionable FunExpr FunExpr FunExpr where
  doDiv = (:/)

-- Det intressanta är att den är Calculable! Num är faktiskt inte det
-- så här blir det spännande.

instance Calculable FunExpr where
  doDif = simplify . derive
  doInteg = simplify . integrate


----------------------------------------
-- Exempel
----------------------------------------

-- Ett flygplan med viss position är nedanstående

g1 = V3 Id (Const 3 :* Exp) Sin ## length

-- Dess hastighet är

g2 :: Quantity (Length `Div` Time) (Vector3 FunExpr)
g2 = diff g1

-- Dess acceleration är

g3 :: Quantity ((Length `Div` Time) `Div` Time) (Vector3 FunExpr)
g3 = diff g2

-- Det väger 120 kg

g4 :: Quantity Mass FunExpr
g4 = Const 120 ## mass

-- Så nettokraften är

g5 :: Quantity (Mass `Mul` ((Length `Div` Time) `Div` Time)) (Vector3 FunExpr)
g5 = g4 *# g3

























































