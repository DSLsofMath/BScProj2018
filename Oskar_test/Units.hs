
module Units
(
)
where

import Data.List

{-
data Quantity v u = Quantity v u

class Unit u where
  mult :: (Unit u1, Unit u2) => u -> u1 -> u2
  mult = unitMult
  --divt :: (Unit u1, Unit u2) => u -> u1 -> u2

instance Unit Length where
instance Unit Time where
instance Unit Mass where
instance (Unit u1, Unit u2) => Unit (Mul u1 u2) where
instance (Unit u1, Unit u2) => Unit (Div u1 u2) where

data Length = Length
data Time = Time
data Mass = Mass

data Mul u1 u2 = Mul u1 u2
data Div u1 u2 = Div u1 u2

type Velocity = Div Length Time

unitMult :: (Unit u1, Unit u2) => u1 -> u2 -> u3
unitMult u1 u2 = Mul u1 u2

--reduce :: (Unit u1, Unit u2) => u1 -> u2
--reduce (Mul (Div Length Time) Time) = Length



massa :: Quantity Double Mass
massa = Quantity 70 Mass

add :: (Num v, Unit u) => Quantity v u -> Quantity v u -> Quantity v u
add (Quantity v1 u) (Quantity v2 _) = Quantity (v1+v2) u

mul :: (Num v, Unit u1, Unit u2, Unit u3) => Quantity v u1 -> Quantity v u2 -> Quantity v u3
mul (Quantity v1 u1) (Quantity v2 u2) = Quantity (v1*v2) (mult u1 u2)

-}

-- En variant där storheter/typer är värden och inte Haskell-typer
-- Nackelden är att då kan inte kompilatorn kolla att rätt

data Unit = Unit [(BaseUnit, Int)]
          deriving (Show)

data BaseUnit = Length
              | Time
              | Mass
              deriving (Eq, Ord, Show)

              
length :: Unit
length = Unit [(Length, 1)]

time :: Unit
time = Unit [(Time, 1)]

mass :: Unit
mass = Unit [(Mass, 1)]

velocity :: Unit
velocity = Unit [(Length, 1), (Time, -1)]

acceleration :: Unit
acceleration = Unit [(Length, 1), (Time, -2)]

weird :: Unit
weird = Unit [(Length, 2), (Length, 1), (Time, -3), (Length, -3), (Mass, 2)]


canonify :: Unit -> Unit
canonify (Unit us) = Unit . reverse $ sorted'
  where
    sorted  = sort us
    grouped = groupBy (\(u1, _) (u2, _) -> u1 == u2) sorted
    summed  = map f grouped
    nonZero = filter (\g@(_, n) -> n /= 0) summed
    sorted' = sortBy (\(_, n1) (_, n2) -> compare n1 n2) nonZero
    
    f us = let (u, _) = head us
               nTot   = sum $ map (\(_, n) -> n) us
           in (u, nTot)


res = canonify weird
