
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dimensions.Quantity.Test
(
)
where

import Prelude hiding (length, div)
import Test.QuickCheck

import Dimensions.TypeLevel
import Dimensions.Quantity

------------------------------------------------------------

-- Only the arithmetic ops on values will be tested.

type Q = Quantity One Rational

genQuantity :: Gen Q
genQuantity = do
  v <- arbitrary
  return (v # one)

instance Arbitrary Q where
  arbitrary = genQuantity

-- Property: addition commutative
prop_addCommutative :: Q -> Q -> Bool
prop_addCommutative q1 q2 = (q1 +# q2) == (q2 +# q1)

-- Property: multiplication and divsion cancel each other out
prop_mulDivCancel :: Q -> Q -> Property
prop_mulDivCancel q1 q2 = (q1 /= (fromInteger 0) # one)
  ==> (q1 *# q2) /# q1 == q2

-- Property: syntactic sugar is correct
prop_sugar :: Rational -> Q -> Bool
prop_sugar v q = v # q == v # one