
module Unit.Syntax
( Unit(..)
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

-- Modellerar sättet man skriver enheter

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

velocity :: Unit
velocity = length :/: time

acceleration :: Unit
acceleration = velocity :/: time

one :: Unit
one = One
