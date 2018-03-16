
-- Vill använda vectors
import Vector
-- Alla vektorer är i Newton

-- Allt annat i Quantity.

fg = V2 0 10
m = 2
alpha = 30
cf = 1

enh_normal :: Double -> Vector2
enh_normal a = V2 (sin a) (cos a)

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
(0.0 x, -20.0 y)                                Odd:    Motsatt gravitation ger något underligt?
*Main> fr (V2 0 10) (pi/2)
(-6.123233995736766e-16 x, 10.0 y)              Good:   90* lutning - faller med G.
*Main> fr (V2 0 10) (pi/3)
(-4.330127018922194 x, 7.499999999999999 y)     
*Main> fr (V2 0 10) (pi/4)
(-5.0 x, 4.999999999999999 y)                   Bad:   45* lutning - 5N både i x och y led. Pyth: 5^2 + 5^2 =/= 10^2
                                                        det borde bli:  100 = a^2 + a^2
                                                                        50 = a^2
                                                                        5*sqrt(2) = a
*Main> fr (V2 0 10) (pi/6)
(-4.330127018922193 x, 2.499999999999999 y)
-}


-- Friktionskonstant - i rörelse:
cr = 0.5

type Speed = Double
type FricConst = Double

frik :: Speed -> FricConst -> Double
frik speed cr = cr * speed

frik_angle :: Speed -> FricConst -> Angle -> Double
frik_angle s cr a = (frik s cr) * (cos a)

enh_plan :: Double -> Vector2
enh_plan a = V2 (sin a) (cos a)

ff :: Speed -> FricConst -> Angle -> Vector2
ff s cr a = scale (frik_angle s cr a) (enh_plan a)

frf :: Vector2 -> Angle -> Speed -> FricConst -> Vector2
frf fa a s cr = (fn fa a) + fa + (ff s cr a)


