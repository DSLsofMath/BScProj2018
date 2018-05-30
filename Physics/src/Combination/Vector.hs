
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module Combination.Vector where

import Combination.Quantity
import Dimensions.TypeLevel
import Prelude              as P hiding (length)

----------------------------------------
-- Än så länge inget nytt, bara modifierat
----------------------------------------

data Vector3 v = V3 v v v

class Vector vector where
  vmap      :: (a -> b)      -> vector a -> vector b
  vzipWith  :: (a -> b -> c) -> vector a -> vector b -> vector c
  vfold     :: (v -> v -> v) -> vector v -> v

instance Vector Vector3 where
  vmap     f (V3 x y z)               = V3 (f x)    (f y)    (f z)
  vzipWith f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  vfold    f (V3 x y z)               = f z $ f x y

instance Show num => Show (Vector3 num) where
  show (V3 x y z) = "(" ++ show x ++ " x, "
                        ++ show y ++ " y, "
                        ++ show z ++ " z)"

add :: (Addable a b c, Vector vec) => vec a -> vec b -> vec c
add = vzipWith doAdd

sub :: (Subable a b c, Vector vec) => vec a -> vec b -> vec c 
sub = vzipWith doSub

scale :: (Multiplicable a b c, Vector vec) => a -> vec b -> vec c
scale factor = vmap (doMul factor)

magnitude :: (Floating num, Vector vec) => vec num -> num
magnitude = sqrt . vfold (+) . vmap (**2)

dotProd :: (Multiplicable a b c, Addable c c c, Vector vec) => vec a -> vec b -> c
dotProd v1 v2 = vfold doAdd $ vzipWith doMul v1 v2

crossProd :: (Multiplicable v v v, Subable v v v) => Vector3 v -> Vector3 v -> Vector3 v
crossProd (V3 x y z) (V3 x' y' z') = V3 a b c
  where
    -- Denna verkar få typerna rätt
    mult :: (Multiplicable v v v) => v -> v -> v
    mult = doMul
    
    subb :: (Subable v v v) => v -> v -> v
    subb = doSub
    
    a = (y `mult` z') `subb` (z `mult` y')
    b = (z `mult` x') `subb` (x `mult` z')
    c = (x `mult` y') `subb` (y `mult` x')

----------------------------------------
-- Instansiering
----------------------------------------

-- Om det en vektor innehåller kan göras något med, så
-- kan även vektorn själv göras det på

instance (Addable a b c) => Addable (Vector3 a) (Vector3 b) (Vector3 c) where
  doAdd = vzipWith doAdd

-- En vektor kan multipliceras på flera sätt

-- Skalning (från vänster)
instance (Multiplicable v v v) => Multiplicable v (Vector3 v) (Vector3 v) where
  doMul = scale

-- Kryssprdoukt
instance (Multiplicable v v v, Subable v v v) => Multiplicable (Vector3 v) (Vector3 v) (Vector3 v) where
  doMul = crossProd

-- Skalärprodukt
instance (Multiplicable v v v, Addable v v v) => Multiplicable (Vector3 v) (Vector3 v) v where
  doMul = dotProd

-- :( v måste vara samma i skalning och skalär, annars blir t2 jobbig
-- Den säger overlapping instances

----------------------------------------
-- Exempelanvändning
----------------------------------------

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

----------------------------------------
-- Analys pass-thorugh
----------------------------------------

-- Gör dem bara komponentvis!

instance (Calculable v) => Calculable (Vector3 v) where
  doDif = vmap doDif
  doInteg = vmap doInteg

-- Det gick snabbt...



































