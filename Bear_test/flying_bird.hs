
import Vector

-- This flying bird is flying at a magnitude and a direction of:

wanted_vel_and_dir = V2 0 5 --Vi vill söderut

wind_vec = V2 2 0  -- Det blåser från väst

bird_vec = wanted_vel_and_dir - wind_vec



-- Keeping constant speed = 5

wind_vec2 = V2 3 0

bird_vec2 = V2 (-3) 4

result_vec = wind_vec2 + bird_vec2

bird_flax_speed = magnitude bird_vec2


--      N           y
--  W       E    -x   x
--      S          -y 

-- Om vi bara vill norrut? Speed 5 i flax.

-- För att inte blåsa österut:
-- måste vi ha V2 3 x
-- samt speeden 5 = magnitude (V2 3 x)
-- eller        5 = sqrt(3^2 + x^2)
--              25 = 3^2 + x^2
--              16 = x^2
--              sqrt(16) = x
--              4 = x
--
-- Dvs          V2 3 4
--
-- Hade vi kunnat få fram detta med Vector.lhs?

--            wind     wanted dir   speed   needed dir
--get_speed :: Vector2 -> Vector2 -> Scalar -> Vector2
--get_speed (


-- 25 = 3^2 + x^2
-- 25 - 3^2 = x^2

-- Hur blir det för andra vinklar? Oo
--
-- oavsett vad vindriktningen är, vill man kompensera för blåst i x led
-- sedan full fart i riktningen.

-- Resultant = fågelflax + blåst
fågelflax :: Vector2 
fågelflax = V2 3 4

blåst = V2 1 (-2)

resultant = fågelflax + blåst


fågelvel = 5.0
--          blåst    fågelspeed
fågelX :: Vector2 -> Double -> Double
fågelX (V2 x y) vel | x > vel = error "Fågeln är inte snabb nog"
fågelX (V2 x y) vel | otherwise = x

-- Om vind x + y är för svårt för fågeln
-- Testar om x led är för svårt
-- Testar om inkluderat y led är för svårt
-- Räknar ut flaxvektor





