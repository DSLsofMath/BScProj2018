
-- Vill använda vectors
import Vector
-- Alla vektorer är i Newton

-- Allt annat i Quantity.

fg = V2 0 10
m = 2
alpha = 30

enh_normal :: Double -> Vector2
enh_normal a = V2 ((sin a)) (cos a)

f_l_ :: Vector2 -> Angle -> Vector2
f_l_ fa a = scale ((magnitude fa) * (cos a)) (enh_normal a)

fn :: Vector2 -> Angle -> Vector2
fn fa a = negate (f_l_ fa a)

-- Friktionsfritt plan:



fr :: Vector2 -> Angle -> Vector2
fr fa a = (fn fa a) + fa

{-| Tester:
*Main> fr (V2 0 10) 0
(0.0 x, 0.0 y)                                  Good:   Inge lutning - står still.
*Main> fr (V2 0 (-10)) 0
(0.0 x, -20.0 y)                                Odd:    Motsatt gravitation ger något underligt? Vi säger fortfarande att normalen är magnituden
*Main> fr (V2 0 10) (pi/2)
(-6.123233995736766e-16 x, 10.0 y)              Good:   90* lutning - faller med G.
*Main> fr (V2 0 10) (pi/3)
(-4.330127018922194 x, 7.499999999999999 y)     
*Main> fr (V2 0 10) (pi/4)
(-5.0 x, 4.999999999999999 y)                   Good:   45* lutning - 5N både i x och y led. Pyth: 5^2 + 5^2 =/= 10^2
                                                        det borde bli:  100 = a^2 + a^2
                                                                        50 = a^2
                                                                        5*sqrt(2) = a

                                                        och det är det ju. xD

*Main> fr (V2 0 10) (pi/6)
(-4.330127018922193 x, 2.499999999999999 y)
-}


-- Friktionskonstant - i rörelse:
us = 0.5
uk = 0.4

type FricConst = Double

-- friktionen 
--  friks = Fn * us,    us = friktion statisk
--  frikk = Fn * uk,    uk = friktion kinetisk
-- 
--  Vi har normalkraften, o beh bara konstanterna.
--  Speed har inget att göra med N för friktion? Dock gäller F*m = Nm = work = J = bugatti bränner däck.
motscalar :: FricConst -> Vector2 -> Scalar
motscalar u f = u * (magnitude f)

-- Från en rörelse eller vekt, fixa komplementet

enh_vekt :: Vector2 -> Vector2
enh_vekt v  | magnitude v == 0 = (V2 0 0)
            | otherwise = scale (1 / (magnitude v)) v


motkrafts :: FricConst -> Scalar -> Vector2 -> Vector2
motkrafts u s v = scale (u * s) (negate (enh_vekt v))

motkraftv :: FricConst -> Vector2 -> Vector2 -> Vector2
motkraftv u n v = scale (u * (magnitude n)) (negate (enh_vekt v))

-- Nu ska vi bara summera

fru :: Vector2 -> Angle -> FricConst -> Vector2
fru fa a u = (fr fa a) + (motkraftv u (fn fa a) (fr fa a))

{-|
Hmm om jag försöker skala riktningsvektorer till sin enhetsvektor så blir det blub med nollvektorn.

> enh_vekt v = scale (1 / (magnitude v)) v 
$ enh_vekt (V2 0 0)
(NaN x, NaN y)

Jag skulle anta att enh_vekt bara gäller då (magnitude v) =/= 0.
-}

-- Fixed nollvektorn.

{-
*Main> fru fg (pi/4) 0
(-5.0 x, 4.999999999999999 y)
*Main> fru fg (pi/4) 1
(1.7763568394002505e-15 x, -1.7763568394002505e-15 y)
*Main> fru fg (pi/4) 0.5
(-2.499999999999999 x, 2.4999999999999987 y)
Ugh? Den är linjär? 1 i friktionskoeff = full stop. alltid?
-}

