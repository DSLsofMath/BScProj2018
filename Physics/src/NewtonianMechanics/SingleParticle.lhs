> {-# LANGUAGE GADTs #-}

> module NewtonianMechanics.SingleParticle where

> import           Calculus.Calculus
> import           Calculus.SyntaxTree
> import           Test.QuickCheck
> import           Vector.Vector as V

Laws
1: A body remains at rest or in uniform motion unless acted upon by a force.
2: A body acted upon by a force moves in such a manner that the time rate of
change of the momentum equals the force.
3: If two bodies exert forces on each other, these forces are equal in
magnitude and opposite in direction.

> type Time    = Double
> type Mass    = FunExpr
> type VectorE = Vector3 FunExpr


> data Particle  = P { pos  :: VectorE -- Position as a function of time, unit m
>                    --, time :: Time   -- Time, unit s
>                    , mass :: Mass   -- Mass, unit kg
>                    } deriving Show

Velocity, derivative of pos with respect to time
unit m*s^-1

> velocity :: Particle -> VectorE
> velocity = vmap D . pos

Acceleration, derivative of velocity with respect to time
unit m*s^-2

> acceleration :: Particle -> VectorE
> acceleration = vmap D . velocity

Force: F = m*a
unit kg * m * s^-1
this is a bit weird

> force :: Particle -> VectorE
> force p = vmap (* m) a
>   where
>     m = mass p
>     a = acceleration p

> type Energy = FunExpr

TODO: Write something here

> square :: VectorE -> FunExpr
> square v = dotProd v v

1/2 m * v^2

> kineticEnergy :: Particle -> Energy
> kineticEnergy p = Const 0.5 * m * v2
>   where
>     m  = mass p
>     v  = velocity p
>     v2 = square v

The work-energy theorem states that for a particle of constant mass *m*, the
total work *W* done on the particle as it moves from position $r_1$ to $r_2$ is
equal to the change in kinetic energy $E_k$ of the particle:

\begin{equation}
  W = \delta E_K = E_{k,2} - E_{k,1} = \frac{1}{2} m (v^2_2 - v^2_1)
\end{equation}

Let's codify this theorem:

> prop_WorkEnergyTheorem :: Mass -> VectorE -> VectorE -> IO Bool
> prop_WorkEnergyTheorem m v1 v2 = prettyEqual deltaEnergy (kineticEnergy displacedParticle)
>   where
>     particle1 = P v1 m -- | Two particles with the same mass
>     particle2 = P v2 m -- | But different position vector
>     -- | E_k,2 - E_k,1
>     deltaEnergy = (kineticEnergy particle2) - (kineticEnergy particle1)
>     displacedParticle = P (v2 - v1) m

> -- Test values
> v1 = V3 (3 :* Id) (2 :* Id) (1 :* Id)
> v2 = V3 2 2 (5 :* Id)
> m  = 5
> p1 = P v1 m
> p2 = P v2 m
> dE = (kineticEnergy p2) - (kineticEnergy p1)
> p3 = P (v2 - v1) m

If a particles position is defined as a vector representing its displacement
from some origin O, then its heigh should be x. Or maybe it should be the
magnitude of the vector, if the gravitational force originates from O. Hmmm


This seems so weird since I don't know what the frame of reference is...

> potentialEnergy :: Particle -> Energy
> potentialEnergy p = undefined
>   where
>     m          = mass p
>     (V3 x _ _) = pos p

\begin{equation}
  F = G \frac{m_1 m_2}{r^2}
\end{equation}
Where *F* is the force, *m1* and *m2* are the masses of the objects interacting,
*r* is the distance between the centers of the masses and *G* is the
gravitational constant.

> type Constant = FunExpr

> gravConst :: Constant
> gravConst = 6.674 * (10 ** (-11))

> lawOfUniversalGravitation :: Particle -> Particle -> FunExpr
> lawOfUniversalGravitation p1 p2 = gravConst * ((m1 * m2) / r2)
>   where
>     m1 = mass p1
>     m2 = mass p2
>     r2 = square $ (pos p2) - (pos p1)

> instance Num FunExpr where
>   (+) = (:+)
>   (*) = (:*)
>   (-) = (:-)
>   fromInteger i = Const (fromInteger i)

> instance Fractional FunExpr where
>   (/) = (:/)
>   fromRational r = Const (fromRational r)

> instance Floating FunExpr where
>   exp a = Exp :. a
>   log a = Exp :. a

TODO!!!! Fix prettyCan $ lawOfUniversalGravitation p1 p2

