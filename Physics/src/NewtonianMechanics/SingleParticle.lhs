> {-# LANGUAGE GADTs #-}

> module NewtonianMechanics.SingleParticle where

> import           Calculus.Calculus
> import           Test.QuickCheck
> import           Vector.Vector as V

Laws
1: A body remains at rest or in uniform motion unless acted upon by a force.
2: A body acted upon by a force moves in such a manner that the time rate of
change of the momentum equals the force.
3: If two bodies exert forces on each other, these forces are equal in
magnitude and opposite in direction.

> type Time   = Double
> type Mass   = Expr
> type VectorE = Vector3 Expr


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

> type Energy = Expr

TODO: Write something here

> square :: VectorE -> Expr
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

> prop_WorkEnergyTheorem :: Mass -> VectorE -> VectorE -> Bool
> prop_WorkEnergyTheorem m v1 v2 = deltaEnergy == kineticEnergy displacedParticle
>   where
>     particle1 = P v1 m -- | Two particles with the same mass
>     particle2 = P v2 m -- | But different position vector
>     -- | E_k,2 - E_k,1
>     deltaEnergy = (kineticEnergy particle2) - (kineticEnergy particle1)
>     displacedParticle = P (v2 - v1) m


< instance Show Particle where
< show particle = "Position: " ++ particlePos  ++ "\n" ++
<                 "Time: "     ++ particleTime ++ "\n" ++
<                 "Mass: "     ++ particleMass
<   where
<     particlePos  = Prelude.show $ pos particle
<     particleTime = Prelude.show $ time particle
<     particleMass = Prelude.show $ mass particle

