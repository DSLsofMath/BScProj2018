
{-# LANGUAGE TypeOperators #-}

module Combination.Combination
    (
    ) where

import Vector.Vector
import Calculus.FunExpr
import Calculus.DifferentialCalc
import Calculus.IntegralCalc
import Dimensions.Quantity2
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

integrateWRTtime :: QC d -> QC (d `Mul` Time)
integrateWRTtime qc = fmap simplify $ fmap integrate qc *# time

-- Calc i Vec i Quan
--------------------

type QVC d = Quantity d (Vector2 FunExpr)

anyVal :: Vector2 FunExpr
anyVal = V2 (Const 1) (Const 1)

lengthQVC      = length' anyVal
massQVC        = mass' anyVal
timeQVC        = time' anyVal
temperatureQVC = temperature' anyVal
currentQVC     = current' anyVal
substanceQVC   = substance' anyVal
luminosityQVC  = luminosity' anyVal
oneQVC         = one' anyVal

s1 :: QVC Length
s1 = V2 (5 :+ Id) (2 :* Id) ## lengthQVC

s2 :: QVC Time
s2 = V2 (Id :* Id) (8) ## timeQVC

-- Socker
(###) :: (FunExpr, FunExpr) -> QVC d -> QVC d
(x, y) ### qvc = V2 x y ## qvc

s3 :: QVC Time
s3 = (Id, Sin) ### timeQVC

addQVC :: QVC d -> QVC d -> QVC d
addQVC = quantityAdd' (vzipWith (+))

simplifyQVC :: QVC d -> QVC d
simplifyQVC = fmap (vmap simplify)

scaleQVC :: Quantity d1 FunExpr -> QVC d2 -> QVC (d1 `Mul` d2)
scaleQVC s qvc = simplifyQVC $ quantityMul' (\fe vec -> scale fe vec) s qvc

divQVC :: Quantity d1 FunExpr -> QVC d2 -> QVC (d1 `Div` d2)
divQVC s qvc = simplifyQVC $ quantityDiv' (\fe vec -> scale (1 :/ fe) vec) s qvc

diffQVC :: QVC d -> QVC (d `Div` Time)
diffQVC qvc = simplifyQVC differentiated
  where
    differentiated = quantityDiv' f (fmap (vmap derive) qvc) timeQVC
    f = vzipWith (/)

-- Ett flygplans position av tiden bestäms av nedan

pos :: QVC Length
pos = (Sin :+ Const 8, Id :* Const 3) ### lengthQVC

-- Vad är hastigheten hos ett flygplan som flyger dubbelt så snabbt, som en funktion av tiden?

velDoub :: QVC (Length `Div` Time)
velDoub = scaleQVC (Const 2 # one) (diffQVC pos)










































