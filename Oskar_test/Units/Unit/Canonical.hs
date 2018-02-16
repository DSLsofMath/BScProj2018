
{-# LANGUAGE InstanceSigs #-}

module Unit.Canonical
( Unit(..)
, BaseUnit(..)
, length
, time
, mass
, one
)
where


import Prelude hiding (length)
import Data.List hiding (length)

import Helper


------------------------------------------------------------
-- Datatypen

-- Modellerar det "kanoniska" sättet enheter kan representeras
-- men som inte är det "naturliga" sättet att skriva dem

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

one :: Unit
one = Unit []

weird :: Unit
weird = Unit [(Length, 2), (Length, 1), (Time, -3), (Length, -3), (Mass, 2)]


------------------------------------------------------------
-- Förenklare

canonify :: Unit -> Unit
canonify (Unit []) = Unit []
canonify (Unit us) = Unit sorted'
  where
    sorted  = sort us
    grouped = groupBy (\(u1, _) (u2, _) -> u1 == u2) sorted
    summed  = map f grouped
    nonZero = filter (\g@(_, n) -> n /= 0) summed
    sorted' = sortBy (\(_, n1) (_, n2) -> compare n1 n2) nonZero

    f us = let (u, _) = head us
               nTot   = sum $ map (\(_, n) -> n) us
           in (u, nTot)

weird' :: Unit
weird' = canonify weird

weird2 :: Unit
weird2 = canonify $ Unit [(Length, 2), (Time, -4), (Mass, -2)]


------------------------------------------------------------
-- Instansiering

-- Så att de vanliga räknesätten kan användas

instance Eq Unit where
  (==) = eqUnit

eqUnit :: Unit -> Unit -> Bool
eqUnit u1 u2 = u1' == u2'
  where
      (Unit u1') = canonify u1
      (Unit u2') = canonify u2

-- General advice: keep instance declarations "trivial" (method = name)
-- and define the actual function |name| elsewhere, like I now did with
-- |eqUnit|.

instance Num Unit where
  u1 + u2
    | u1 == u2 = u1
    | otherwise = error "Units that are added must be the same"
  (Unit u1) * (Unit u2) = canonify . Unit $ u1 ++ u2
  negate = id
  abs = id
  signum = id
  fromInteger _ = one

instance Fractional Unit where
  recip (Unit u) = canonify . Unit $ map (\(u', n) -> (u', -n)) u
  fromRational _ = one

instance Floating Unit where
  pi    = one
  exp   = id
  log   = id
  sin   = id
  cos   = id
  asin  = id
  acos  = id
  atan  = id
  sinh  = id
  cosh  = id
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


------------------------------------------------------------
-- Papperssyntax

-- Nu går det att skriva in som man gör på papper
-- och sedan få ett snyggt resultat

vel  = length / time
acc1 = length / time / time
acc2 = length / (time * time)
acc3 = vel / time

-- Grejen är dock att hur det händer sker i "bakom kulisserna"
-- och då kanske poängen med ett DSL för pedagogik försvinner
