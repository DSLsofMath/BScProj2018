
module Unit.Syntax where

import Prelude hiding (length)
import Data.List hiding (length)

import Helper

data Unit = One
          | Length Int
          | Time Int
          | Mass Int
          | Unit :*: Unit
          | Unit :/: Unit
          deriving (Eq, Ord, Show)

length :: Unit
length = Length 1

time :: Unit
time = Time 1

mass :: Unit
mass = Mass 1

area :: Unit
area = Length 2
area' = Length 1 :*: Length 1

multiUnit u 1 = u
multiUnit u n = u :*: (multiUnit (n-1) u)

area' :: Unit
area' = length :*: length

velocity :: Unit
velocity = length :/: time

acceleration :: Unit
acceleration = velocity :/: time

-- Gör allt till _en_ division mellan _flera_ multiplikationer
-- (täljare) med _flera_ multiplikationer (nämnare)
numDen :: Unit -> ([Unit], [Unit])
numDen One = ([One], [])
numDen (Length n) = ([Length n], [])
numDen (Time n) = ([Time n], [])
numDen (Mass n) = ([Mass n], [])
numDen (u1 :*: u2) = (num1 ++ num2, den1 ++ den2)
  where
    (num1, den1) = numDen u1
    (num2, den2) = numDen u2
numDen (u1 :/: u2) = (num1 ++ den2, num2 ++ den1)
  where
    (num1, den1) = numDen u1
    (num2, den2) = numDen u2

t = ([Mass 1, Mass 3], [Time 1, Mass 2])

--showUnit :: ([Unit], [Unit]) -> String
showUnit (num, den) = summed
  where
    den' = map f den
      where
        f One = One
        f (Length n) = Length (-n)
        f (Time n)   = Time (-n)
        f (Mass n)   = Mass (-n)
    sorted  = sort (num ++ den')
    grouped = groupBy f sorted
      where
        f One One               = True
        f (Length _) (Length _) = True
        f (Time   _) (Time   _) = True
        f (Mass   _) (Mass   _) = True
        f _          _          = False
    summed = map f grouped
      where
        f ls@(One:_)        = One
        f ls@((Length _):_) = Length (sum $ map (\(Length n) -> n) ls)
        f ls@((Time   _):_) = Time   (sum $ map (\(Time   n) -> n) ls)
        f ls@((Mass   _):_) = Mass   (sum $ map (\(Mass   n) -> n) ls)
    
