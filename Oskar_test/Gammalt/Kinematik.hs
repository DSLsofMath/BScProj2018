
module Kinematik
(
)
where

import Data.Ratio

-- Om acceleration är konstant gäller nedanstående samband
-- Vf  = Vi + a*t
-- Xf  = Xi + 0,5*(Vf-Vi)*t
-- Xf  = Xi + Vi*t + 0,5*a*t²
-- Xf² = Vi² 0 2*x*(Xf-Vi)

-- Utgå ifrån a=dv/dt och v=dx/dt momentant

-- Konstuktorer som grunden över samand
-- dessa kan omvandlas till andra samband

-- Grunddatatyper

data Distance r = Distance r
                | SumD [Distance r]
                | Velocity r `VmT` Time r
                deriving (Eq, Show)

data Time r = Time r
            | SumT [Time r]
            | Distance r `DdV` Time r
            deriving (Eq, Show)

-- Kompositdatatyper

-- v = x/t
data Velocity r = Distance r `DdT` Time r
                | SumV [Velocity r]
                deriving (Eq, Show)

-- Någon annan datatypsslag

data DistanceM r = DistanceM 
                
-- Om v = dx/dt momentant. Visa att om v är konstant, så
-- är Dx=v*Dt

data EqD r = (Distance r) `EqD` (Distance r)
data EqT r = (Time r)     `EqT` (Time r)
data EqV r = (Velocity r) `EqV` (Velocity r)

multT1 :: EqV r -> Time r -> EqD r
multT1 (v1 `EqV` v2) t = (v1 `VmT` t) `EqD` (v2 `VmT` t)

multT2 :: Velocity r -> Time r -> Distance r
multT2 = VmT                    

divT :: EqD r -> Time r -> EqV r
divT (d1 `EqD` d2) t = (d1 `DdT` t) `EqV` (d2 `DdT` t)


-- Exempel

d :: Distance (Ratio Integer)
d = Distance 20

t :: Time (Ratio Integer)
t = Time 5

v = d `DdT` t

-- Förenkling



