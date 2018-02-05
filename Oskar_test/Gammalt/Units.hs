
{-# LANGUAGE InstanceSigs #-}

module Units
(
)
where

import Prelude hiding (length)
import Data.List hiding (length)

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

len :: (Integral n) => [a] -> n
len [] = 0
len (a:as) = 1 + len as

-- En variant där storheter/typer är värden och inte Haskell-typer
-- Nackelden är att då kan inte kompilatorn kolla att rätt

newtype Unit = Unit [(BaseUnit, Int)]

data BaseUnit = Length
              | Time
              | Mass
              deriving (Eq, Ord)

              
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

weird' = canonify weird

weird2 = canonify $ Unit [(Length, 2), (Time, -4), (Mass, -2)]


canonify :: Unit -> Unit
canonify (Unit []) = Unit []
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

instance Eq Unit where
  (==) :: Unit -> Unit -> Bool
  u1 == u2 = u1' == u2'
    where
      (Unit u1') = canonify u1
      (Unit u2') = canonify u2

instance Num Unit where
  u1 + u2
    | u1 == u2 = u1
    | otherwise = error "Units that are added must be the same"
  (Unit u1) * (Unit u2) = canonify . Unit $ u1 ++ u2      
  negate = id
  abs = id
  signum = id
  fromInteger _ = Unit []

instance Fractional Unit where
  recip (Unit u) = canonify . Unit $ map (\(u', n) -> (u', -n)) u
  fromRational _ = Unit []

instance Floating Unit where
  pi = Unit []
  exp = id
  log = id
  sin = id
  cos = id
  asin = id
  acos = id
  atan = id
  sinh = id
  cosh = id
  asinh = id
  acosh = id
  atanh = id


instance Show BaseUnit where
  show Length = "m"
  show Time   = "s"
  show Mass   = "kg"

instance Show Unit where
  show = showUnit

showUnitTuple :: (BaseUnit, Int) -> String
showUnitTuple (u, 1) = show u
showUnitTuple (u, n) = show u ++ "^" ++ show n

showUnit :: Unit -> String
showUnit (Unit []) = ""
showUnit (Unit us)
  | null negStrs = posStr
  | otherwise    = posStr ++ "/" ++ negStr'
  where
    pos = filter (\(_, n) -> n > 0) us
    neg = filter (\(_, n) -> n < 0) us
    neg' = map (\(u, n) -> (u, -n)) neg
    
    posStrs = map showUnitTuple pos
    negStrs = map showUnitTuple neg'
    
    posStr = if null posStrs
             then ""
             else foldl (\strs str -> str ++ "*" ++ strs) 
                        (head posStrs) 
                        (tail posStrs)
    (left, right) = if len negStrs > 1
                    then ("(", ")")
                    else ("", "")
    negStr = if null negStrs
             then ""
             else foldl (\strs str -> str ++ "*" ++ strs) 
                        (head negStrs) 
                        (tail negStrs)
    negStr' = left ++ negStr ++ right

{-
data Extra = Kg Double
           | G Double
           deriving Show
           
hej x u = u x

(#) x u = u x
-- Skriva in uttyrck så nära papperssyntax som möjligt

x :+: u = u x

-}

data Prefix = P'  -- piko
            | N'  -- nano
            | U'  -- mikro
            | M'  -- milli
            | One -- "enhets-prefixet"
            | K   -- Kilo
            | M   -- Mega
            | G   -- Giga
            | T   -- Tera
            | P   -- Peta
            deriving (Eq, Ord)

-- Idé: evaluerare för SI, evaluerare för något annat ...

instance Show Prefix where
  show P' = "p"
  show N' = "n"
  show U' = "u"
  show M' = "m"
  show One = ""
  show K = "K"
  show M = "M"
  show G = "G"
  show T = "T"
  show P = "P"


--              Storhet Mätetal prefix enhet
data Quantity = Quantity Double Prefix Unit


