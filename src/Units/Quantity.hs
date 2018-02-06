
module Quantity
(
)
where

import Prelude hiding (length)
import Helper

import Unit
import Prefix


--              Storhet Mätetal prefix enhet
data Quantity r = Quantity r Prefix Unit
                deriving (Show)


off :: r -> Unit -> Quantity r
r `off` u = Quantity r One u

instance (Eq r) => Eq (Quantity r) where
  (Quantity r1 _ u1) == (Quantity r2 _ u2) = r1 == r2 && u1 == u2

instance (Num r) => Num (Quantity r) where
  (Quantity r1 _ u1) + (Quantity r2 _ u2) = Quantity (r1+r2) One (u1+u2)
  (Quantity r1 _ u1) * (Quantity r2 _ u2) = Quantity (r1*r2) One (u1*u2)

l1 = 5 `off` length
l2 = 3 `off` length
yta = l1 * l2
t1 = 8 `off` time
