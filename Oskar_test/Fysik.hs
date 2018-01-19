
{-# LANGUAGE InstanceSigs, UndecidableInstances #-}

module Fysik
( Acceleration(..)
, Mass(..)
, Force(..)
)
where

-- Syntax-träd hade kanske varit något?

data Acceleration r = Acceleration r
                    | (Force r) `FdM` (Mass r)
                    | SumA [Acceleration r]
                    | ScaleA r (Acceleration r)
                    deriving (Show, Eq, Ord)
                    
data Mass r = Mass r
            | (Force r) `FdA` (Acceleration r)
            | SumM [Mass r]
            | ScaleM r (Mass r)
            deriving (Show, Eq, Ord)

data Force r = Force r
             | (Acceleration r) `AmM` (Mass r)
             | SumF [Force r]
             | ScaleF r (Force r)
             deriving (Show, Eq, Ord)

-- Ett exempel
-- Lucas drar med en kraft på 50 N på en låda uppåt
-- Samtidigt påverkar gravitationen neråt
-- Lådan har massan 4 kg
-- Beräkna accelerationen på den

g :: Acceleration Double
g = Acceleration (-9.82)

lucasKraften :: Force Double
lucasKraften = Force 50

ladan :: Mass Double
ladan = Mass 4

tyngdKraft :: Force Double
tyngdKraft = g `AmM` ladan

nettoKraft :: Force Double
nettoKraft = SumF [lucasKraften, tyngdKraft]

acceleration :: Acceleration Double
acceleration = nettoKraft `FdM` ladan

-- Snygga till operationerna

instance (Num r) => Num (Acceleration r) where
  (+) :: Acceleration r -> Acceleration r -> Acceleration r
  a1 + a2 = SumA [a1, a2]
  
  (*) :: Acceleration r -> Acceleration r -> Acceleration r
  _ * _ = error "Multiplication of accelerations is meaningless"
  
  fromInteger :: Integer -> Acceleration r
  fromInteger n = Acceleration (fromInteger n)
  
  negate :: Acceleration r -> Acceleration r
  negate a = ScaleA (-1) a
  
  abs = undefined
  signum = undefined

instance (Num r) => Num (Mass r) where
  (+) :: Mass r -> Mass r -> Mass r
  a1 + a2 = SumM [a1, a2]
  
  (*) :: Mass r -> Mass r -> Mass r
  _ * _ = error "Multiplication of masses is meaningless"
  
  fromInteger :: Integer -> Mass r
  fromInteger n = Mass (fromInteger n)
  
  negate :: Mass r -> Mass r
  negate _ = error "Negation of mass is meaningless"
  
  abs = undefined
  signum = undefined
  
instance (Num r) => Num (Force r) where
  (+) :: Force r -> Force r -> Force r
  a1 + a2 = SumF [a1, a2]
  
  (*) :: Force r -> Force r -> Force r
  _ * _ = error "Multiplication of Forces is meaningless"
  
  fromInteger :: Integer -> Force r
  fromInteger n = Force (fromInteger n)
  
  negate :: Force r -> Force r
  negate a = ScaleF (-1) a
    
  abs = undefined
  signum = undefined

-- Evaluarare

evalA :: (Fractional r) => Acceleration r -> r
evalA (Acceleration a) = a
evalA (f `FdM` m) = evalF f / evalM m
evalA (SumA list) = sum $ map evalA list
evalA (ScaleA s a) = s * evalA a

evalM :: (Fractional r) => Mass r -> r
evalM (Mass m) = m
evalM (f `FdA` a) = evalF f / evalA a
evalM (SumM list) = sum $ map evalM list
evalM (ScaleM s m) = s * evalM m

evalF :: (Fractional r) => Force r -> r
evalF (Force f) = f
evalF (a `AmM` m) = evalA a * evalM m
evalF (SumF list) = sum $ map evalF list
evalF (ScaleF s f) = s * evalF f

{- Denna bit kompilerar inte

class Evaluatable e where
  eval :: e r -> r

instance Evaluatable Acceleration where
  eval :: Acceleration r -> r
  eval = evalA

-}
  
-- Ett annat exempel

kraft1 :: Force Double
kraft1 = 90

kraft2 :: Force Double
kraft2 = 130

kraft3 :: Force Double
kraft3 = -20

nettoKraften :: Force Double
nettoKraften = kraft1 + kraft2 + kraft3

massan :: Mass Double
massan = 7

accelerationen :: Acceleration Double
accelerationen = nettoKraften `FdM` massan
-- Vilken division som helst får inte göras här
-- Det måste vara division av kraft med massa
-- (Eller senare också t.ex. hastighet dividerat med sekund)

accelerationenSiffra :: Double
accelerationenSiffra = evalA accelerationen