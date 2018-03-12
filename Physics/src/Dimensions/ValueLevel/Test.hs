
module Dimensions.ValueLevel.Test
(
)
where

import Prelude hiding (length, div)
import Test.QuickCheck
import Data.List

import Dimensions.ValueLevel

------------------------------------------------------------

-- Generator for arbitrary value-level dimensions
genDim :: Gen Dim
genDim = do
  le <- arbitrary
  ma <- arbitrary
  ti <- arbitrary
  cu <- arbitrary
  te <- arbitrary
  su <- arbitrary
  lu <- arbitrary
  return (Dim le ma ti cu te su lu)

instance Arbitrary Dim where
  arbitrary = genDim

-- Property: multiplication is commutative
prop_mulCommutative :: Dim -> Dim -> Bool
prop_mulCommutative d1 d2 = d1 `mul` d2 == d2 `mul` d1

-- Property: multiplication is associative
prop_mulAssociative :: Dim -> Dim -> Dim -> Bool
prop_mulAssociative d1 d2 d3 = d1 `mul` (d2 `mul` d3) ==
  (d1 `mul` d2) `mul` d3

-- Property: `one` is a unit for multiplication
prop_mulOneUnit :: Dim -> Bool 
prop_mulOneUnit d = d == one `mul` d

-- Property: dividing twice results in no change
prop_divTwice d1 d2 = d1 `div` (d1 `div` d2) == d2

-- Property: multiplication and division cancel each other out
prop_mulDivCancel d1 d2 = (d1 `mul` d2) `div` d1 == d2

-- Property: dividing by `one` does noting
prop_divOne d = d `div` one == d

-- Property: multiplication same as division by inverse
prop_mulDivInv d1 d2 = d1 `mul` d2 == 
  d1 `div` (one `div` d2)

------------------------------------------------------------

-- Property: pretty-printed has correct of a dimension
prop_correctDim :: Integer -> Dim -> Bool
prop_correctDim i d = prop_correctDim' (i `mod` 7) d (show d)

prop_correctDim' :: Integer -> Dim -> String -> Bool
prop_correctDim' 0 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' le "m" str
prop_correctDim' 1 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' ma "kg" str
prop_correctDim' 2 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' ti "s" str
prop_correctDim' 3 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' cu "A" str
prop_correctDim' 4 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' te "K" str
prop_correctDim' 5 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' su "mol" str
prop_correctDim' 6 (Dim le ma ti cu te su lu) str =
  prop_correctDim'' lu "cd" str

prop_correctDim'' :: Integer -> String -> String -> Bool
prop_correctDim'' exp unit str
  | exp == 0  = not $ isInfixOf unit str
  | exp == 1  = isInfixOf unit num
  | exp == -1 = isInfixOf unit den
  | exp > 1   = isInfixOf (unit ++ "^" ++ show exp) num
  | exp < -1  = isInfixOf (unit ++ "^" ++ show (abs exp)) den
  where
    num = takeWhile(/='/') str
    den = dropWhile(/='/') str

-- TODO: Check that it only occurs once.
