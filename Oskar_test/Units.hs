
module Units
(
)
where

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

