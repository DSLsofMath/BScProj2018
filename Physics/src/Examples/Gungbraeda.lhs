Task 3, exam 2017-01-13

> import Dimensions.TypeLevel
> import Dimensions.Quantity
> import Prelude hiding (length)

> beam_M = 1.0 # mass
> m1 = 2.0 # mass
> m2 = 5.0 # mass
> d = 0.75 # length
> beam_L = 5.0 # length
> two = 2.0 # one

![Gungbraede](Gungbraede.png){.float-img-left}

The goal is to achive balance on the teeter, by adjusting the distance x.
The force on the teeter from a weight is proportional to the weights distance from the anchor point. 

The beam's density is equally distributed.

\begin{align}
  F_{torque} = mass \cdot length
\end{align} 

Direct implication:

> beam_left_L = (beam_L /# two) +# d        -- Beam's left part length
> beam_right_L = beam_L -# beam_left_L      -- Beam's right part length

We want to balance out the torques, and for that we need to setup some equations.

> m1_torque = m1 *# beam_left_L             -- m1 torque

> -- The beams internal torque, based on the parts length and mass
> beam_L_torque = ((beam_left_L /# beam_L) *# beam_M) *# (beam_left_L /# two)

> beam_H_torque = ((beam_right_L /# beam_L) *# beam_M) *# (beam_right_L /# two)


m2_torque = m2 * x


The two parts should balance out:

\begin{align}
    left-hand side = right-hand side
\end{align}

m1_torque + beam_L_torque = m2_torque + beam_H_torque

m1_torque + beam_L_torque - beam_H_torque  = m2_torque

(m1_torque + beam_L_torque - beam_H_torque) / m2  = x

> x = (m1_torque +# beam_L_torque -# beam_H_torque) /# m2

Security check:

Is 0 < x < L/2 - d?

> m2_torque = m2 *# x

> vL = m1_torque +# beam_L_torque
> hL = m2_torque +# beam_H_torque


