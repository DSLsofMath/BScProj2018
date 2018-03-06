
-- Importera deep embedding och Dimensions
--import Dimensions.ValueLevel
import Dimensions.Quantity
import Prelude hiding (length, div)
asd x = x + 1


-- Vi skulle vilja ha showDims, i answer också

-- Freefall
-- Hur lång tid tills bollen slår i marken?

-- S = v*t + a*t² / 2
--
-- Säg begynnelsehastighet = 0
--
-- S = a*t² / 2
--
-- 2*S / a = t²
-- sqrt(2*S / a) = t

-- Sträckan?
--freefall_s :: Double -> Double -> Double -> (Double, Dims )
freefall_s v0 a t = (v0 * t + ((a * t**2) / 2), length )


-- Tiden?
freefall_t s a = sqrt (2 * s / a)




-- Möjl additions: exportera showDims?
--                 vectors?

-- En adderare av dimensioner

-- Oskar har typ många saker i Quantity.lhs som jag bör läsa.
--
