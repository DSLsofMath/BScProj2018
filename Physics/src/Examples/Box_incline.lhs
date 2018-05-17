
Box on an incline
=================

> import Vector.Vector

All vectors are in newton.

![Incline](incline.png){.float-img-left}

Notation:
fg = gravitational accelleration
m = mass of box

> fg = V2 0 (-10)
> m = 2

> unit_normal :: Double -> Vector2 Double
> unit_normal a = V2 (cos a) (sin a)

Force against the incline from the box:

> f_l_ :: Vector2 Double -> Angle -> Vector2 Double
> f_l_ fa a = scale ((magnitude fa) * (cos a)) (unit_normal (a-(pi/2)))

The normal against the incline:

> fn :: Vector2 Double -> Angle -> Vector2 Double
> fn fa a = negate (f_l_ fa a)

Frictionfree incline:

Force resultant:

> fr :: Vector2 Double -> Angle -> Vector2 Double
> fr fa a = (fn fa a) + fa


** Tests:**
------------

*Main> fr (V2 0 10) 0

(0.0 x, 0.0 y)                                  

Good:  No inclination - stands still.


*Main> fr (V2 0 (-10)) 0

(0.0 x, -20.0 y)                                

Odd:    Motsatt gravitation ger något underligt? Vi säger fortfarande att normalen är magnituden


*Main> fr (V2 0 10) (pi/2)

(-6.123233995736766e-16 x, 10.0 y)              

Good:   90* lutning - faller med G.

*Main> fr (V2 0 10) (pi/3)

(-4.330127018922194 x, 7.499999999999999 y)     

*Main> fr (V2 0 10) (pi/4)

(-5.0 x, 4.999999999999999 y)                   

*Main> fr (V2 0 10) (pi/6)

(-4.330127018922193 x, 2.499999999999999 y)


**Frictionconstant - in motion:**

\begin{align}
  F_{friction} = \mu * F_{normal} \iff \mu = \frac{F_{friction}}{F_{normal}}
\end{align}

> us = 0.5
> uk = 0.4

Add image how friction depends if there is movement.

> type FricConst = Double

Friction: 

 friks = Fn * us,    us = friction static

 frikk = Fn * uk,    uk = friction kinetic


We have the normal force and only needs the constants.

The current speed does not affect the friction. However F*M = Nm = work = J -> racer cars burn tires.

> motscalar :: FricConst -> Vector2 Double -> Scalar
> motscalar u f = u * (magnitude f)

Från en rörelse eller vekt, fixa komplementet


> enh_vekt :: Vector2 Double -> Vector2 Double
> enh_vekt v  | magnitude v == 0 = (V2 0 0)
>             | otherwise = scale (1 / (magnitude v)) v
> 
> 
> motkrafts :: FricConst -> Scalar -> Vector2 Double -> Vector2 Double
> motkrafts u s v = scale (u * s) (negate (enh_vekt v))
> 
> motkraftv :: FricConst -> Vector2 Double -> Vector2 Double -> Vector2 Double
> motkraftv u n v = scale (u * (magnitude n)) (negate (enh_vekt v))

Now we just need to sum the force vectors:

> fru :: Vector2 Double -> Angle -> FricConst -> Vector2 Double
> fru fa a u = (fr fa a) + (motkraftv u (fn fa a) (fr fa a))
> 
> fru' :: Vector2 Double -> Angle -> FricConst -> Vector2 Double
> fru' fa a u = (motkraftv u (fn fa a) (fr fa a))




Hmm om jag försöker skala riktningsvektorer till sin enhetsvektor så blir det blub med nollvektorn.

enh_vekt v = scale (1 / (magnitude v)) v 
$ enh_vekt (V2 0 0)
(NaN x, NaN y)

Jag skulle anta att enh_vekt bara gäller då (magnitude v) =/= 0.


Fixed nollvektorn.


*Main> fru fg (pi/4) 0
(-5.0 x, 4.999999999999999 y)
*Main> fru fg (pi/4) 1
(1.7763568394002505e-15 x, -1.7763568394002505e-15 y)
*Main> fru fg (pi/4) 0.5
(-2.499999999999999 x, 2.4999999999999987 y)
Ugh? Den är linjär? 1 i friktionskoeff = full stop. alltid?

När är isf motkraften = fallkraften? 
*Main> fru fg 0 5
(0.0 x, 0.0 y)
*Main> fru fg 0 1
(0.0 x, 0.0 y)
*Main> fru fg (pi/2) 10
(-6.123233995736762e-16 x, 9.999999999999995 y)

Hmm?

*Main> fru fg (pi/6) 1
(3.169872981077808 x, -1.8301270189221936 y)
*Main> fru fg (pi/6) 0
(-4.330127018922193 x, 2.499999999999999 y)
*Main> fru fg (pi/6) 100000
(749995.6698729811 x, -433010.2018922193 y)

Den statiska friktionen är konstig. Den borde stå still vid låg vinkel o hög friktion.

Jag summerar ju visserligen krafterna, så det är nog något lurt med friktionshanteringen.

Tests:

fr
*Main> fr fg (pi/3)
(-4.330127018922194 x, 7.499999999999999 y)
*Main> fr fg (pi/6)
(-4.330127018922193 x, 2.499999999999999 y)


fru
*Main> fru fg (pi/3) 1
(-1.8301270189221928 x, 3.1698729810778055 y)
*Main> fru fg (pi/6) 1
(3.169872981077808 x, -1.8301270189221936 y)



fru'
*Main> fru' fg (pi/3) 1
(2.500000000000001 x, -4.330127018922194 y)
*Main> fru' fg (pi/6) 1
(7.500000000000001 x, -4.330127018922193 y



wtf händer? Hur kan fr ha samma x-vektor för två olika vinklar inom samma kvadrant?

fn

*Main> fn fg (pi/3)
(-4.330127018922194 x, -2.500000000000001 y)
*Main> fn fg (pi/6)
(-4.330127018922193 x, -7.500000000000001 y)


