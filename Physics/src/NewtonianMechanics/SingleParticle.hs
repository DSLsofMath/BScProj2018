{-# LANGUAGE GADTs #-}

module NewtonianMechanics.SingleParticle where

import           Calculus.Calculus
--import           Vector.Vector

-- Laws
-- 1: A body remains at rest or in uniform motion unless acted upon by a force.
-- 2: A body acted upon by a force moves in such a manner that the time rate of
-- change of the momentum equals the force.
-- 3: If two bodies exert forces on each other, these forces are equal in
-- magnitude and opposite in direction.

type Time   = Double
type Mass   = Expr
type Vector = Vector3 Expr

data Vector3 num = V3 num num num

data Particle  = P { pos  :: Vector -- Position as a function of time, unit m
                   , time :: Time   -- Time, unit s
                   , mass :: Mass   -- Mass, unit kg
                   }

instance Functor Vector3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

-- Velocity, derivative of pos with respect to time
-- unit m*s^-1
velocity :: Particle -> Vector
velocity = fmap D . pos

-- Acceleration, derivative of velocity with respect to time
-- unit m*s^-2
acceleration :: Particle -> Vector
acceleration = fmap D . velocity

-- Force: F = m*a
-- unit kg * m * s^-1
-- this is a bit weird
force :: Particle -> Vector
force p = fmap (* m) a
  where
    m = mass p
    a = acceleration p

(>*<) :: Vector -> Vector -> Expr
(V3 x1 y1 z1) >*< (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

type Energy = Expr
-- 1/2 m * v^2
kineticEnergy :: Particle -> Energy
kineticEnergy p = Const 0.5 * m * (v >*< v)
  where
    m = mass p
    v = pos p



