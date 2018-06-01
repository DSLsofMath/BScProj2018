Box on an incline
=================

> import Vector.Vector

All forces are represented in the form of a vector in this example.

![Incline](incline.png){.float-img-left}

Notation:
$$ fg = gravitational\ accelleration $$

$$ m = mass\ of\ box $$

> fg = V2 0 (-10)
> m = 2

A unit vector that we get from a degree $a$.

> unit_normal :: Angle -> Vector2 Double
> unit_normal angle = V2 (cos angle) (sin angle)

Force against the incline $F_{\perp}$ from the box (negative in y-axis).

> f_l_ :: Vector2 Double -> Angle -> Vector2 Double
> f_l_ fa angle = scale ((magnitude fa) * (cos angle)) (unit_normal (angle-(pi/2)))

The normal force $F_{n} = - F_{\perp}$ supporting the box from the incline (positive in y-axis):

> fn :: Vector2 Double -> Angle -> Vector2 Double
> fn fa angle = negate (f_l_ fa angle)

The resulting force is then the normal force from the incline plus the gravitational force.
$$ F_{r} = F_{n} + F_{g} $$

> fr :: Vector2 Double -> Angle -> Vector2 Double
> fr fa angle = (fn fa angle) + fa

With friction:

$$  F_{friction} = \mu * F_{normal} \iff \mu = \frac{F_{friction}}{F_{normal}} $$

There are two different kinds of friction. One where the object is standing still on the surface, and the other where the object is sliding on the surface. In the case where the object is standing still, it remains still until the force applied on the object is greater than the maximum possible friction between the object and the surface. Once that level is surpassed, the object starts to slide.

When the object is sliding the maximum friction between the surface and the object is slightly less, as illustrated in the figure below.

![](friction.png){.float-img-left}

$$ F_{static\ friction} = \mu_{static} \cdot F_{normal} $$
$$ F_{kinetic\ friction} = \mu_{kinetic} \cdot F_{normal} $$

We have the normal force against the incline and only need example constants.

> type FricConst = Double
> us = 0.5
> uk = 0.4

Från en rörelse eller vekt, fixa komplementet

> unit_vec :: Vector2 Double -> Vector2 Double
> unit_vec v  | magnitude v == 0 = (V2 0 0)
>             | otherwise = scale (1 / (magnitude v)) v

> motkrafts :: FricConst -> Scalar -> Vector2 Double -> Vector2 Double
> motkrafts u s v = scale (u * s) (negate (unit_vec v))

> motkraftv :: FricConst -> Vector2 Double -> Vector2 Double -> Vector2 Double
> motkraftv u normal v = scale (u * (magnitude normal)) (negate (unit_vec v))

Now we just need to sum the force vectors into $F_{r}$:

> fru :: Vector2 Double -> Angle -> FricConst -> Vector2 Double
> fru fa a u = (fr fa a) + (motkraftv u (fn fa a) (fr fa a))


