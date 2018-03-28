> module NewtonianMechanics.SingleParticle where

> import           Calculus.Calculus
> import           Calculus.SyntaxTree
> import           Test.QuickCheck
> import           Vector.Vector as V

Laws:

- A body remains at rest or in uniform motion unless acted upon by a force.
- A body acted upon by a force moves in such a manner that the time rate of change of the momentum equals the force.
- If two bodies exert forces on each other, these forces are equal in magnitude and opposite in direction.

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

Forces & Newton's second law
------------------------------

This law expresses the relationship between force and momentum and is
as follows:
\begin{equation}
  \vec{F} = \frac{d \vec{p}}{d t} = \frac{d(m \cdot \vec{v})}{d t}
\end{equation}

The quantity $m \cdot v$ is what we mean when we say momentum. So the law
states that the net force on a particle is equal to the rate of change of the
momentum with respect to time. And since the definition of acceleration is $a =
\frac{d \vec{v}}{d t}$ we can write this law in a more familiar form, namely:

\begin{equation}
  \vec{F} = m \cdot \vec{a}
\end{equation}

And thus if the particle is accelerating we can calculate the net force that
must be acting on it, in code this would be:

> force :: Particle -> VectorE
> force p = vmap (* m) a
>   where
>     m = mass p
>     a = acceleration p

> type Energy = FunExpr

Where the acceleration of particle is found by deriving the velocity of that
same particle with respect to $t$:

> acceleration :: Particle -> VectorE
> acceleration = vmap D . velocity

TODO: Write something here

> square :: VectorE -> FunExpr
> square v = dotProd v v

Work and energy
---------------------

If a constant force $\vec{F}$ is applied to a particle that moves from
position $\vec{r_1}$ to $\vec{r_2}$ then the *work* done by the force is defined
as the dot product of the force and the vector of displacement.

\begin{equation}
  W = \vec{F} \cdot \Delta \vec{r}
\end{equation}

where $\Delta \vec{r} = \vec{r_2} - \vec{r_1}$.

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
  W = \Delta E_K = E_{k,2} - E_{k,1} = \frac{1}{2} m (\vec{v_2}^2 - \vec{v_1}^2)
\end{equation}

Let's codify this theorem:

> prop_WorkEnergyTheorem :: Mass -> VectorE -> VectorE -> IO Bool
> prop_WorkEnergyTheorem m v1 v2 = prettyEqual deltaEnergy (kineticEnergy displacedParticle)
>   where
>     particle1 = P v1 m -- | Two particles with the same mass
>     particle2 = P v2 m -- | But different position vector
>     -- |          E_k,2                     - E_k,1
>     deltaEnergy = kineticEnergy particle2 - kineticEnergy particle1
>     displacedParticle = P (v2 - v1) m

> -- Test values
> v1 = V3 (3 :* Id) (2 :* Id) (1 :* Id)
> v2 = V3 2 2 (5 :* Id)
> m  = 5
> p1 = P v1 m
> p2 = P v2 m
> dE = kineticEnergy p2 - kineticEnergy p1
> p3 = P (v2 - v1) m

Law of universal gravitation
-------------------------------------
Newton's law of universal gravitation states that a particle attracts every
other particle in the universe with a force that is directly proportional to
the product of their masses, and is inversely proportional to the square
of the distance between their centers.

This means that every particle with mass attracts every other particle with
mass by a force pointing along the line intersecting both points.

There is an equation for calculating the magnitude of this force which
states with math what we stated in words above:

\begin{equation}
  F = G \frac{m_1 m_2}{r^2}
\end{equation}
Where *F* is the maginitude of the force, *m1* and *m2* are the masses of the
objects interacting, *r* is the distance between the centers of the masses and
*G* is the gravitational constant.

The gravitational constant has been finely approximated through experiments
and we can state it out code like this:

> type Constant = FunExpr
>
> gravConst :: Constant
> gravConst = 6.674 * (10 ** (-11))

Now we can codify the law of universal gravitation using our definition
of particles.

> lawOfUniversalGravitation :: Particle -> Particle -> FunExpr
> lawOfUniversalGravitation p1 p2 = gravConst * ((m1 * m2) / r2)
>   where
>     m1 = mass p1
>     m2 = mass p2
>     r2 = square $ pos p2 - pos p1

If a particles position is defined as a vector representing its displacement
from some origin O, then its heigh should be x. Or maybe it should be the
magnitude of the vector, if the gravitational force originates from O. Hmmm

This seems so weird since I don't know what the frame of reference is...

> potentialEnergy :: Particle -> Energy
> potentialEnergy p = undefined
>   where
>     m          = mass p
>     (V3 x _ _) = pos p

TODO!!!! Fix prettyCan $ lawOfUniversalGravitation p1 p2
