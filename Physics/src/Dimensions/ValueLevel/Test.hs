
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
  prop_correctDim'' le "me" str
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
  | exp == 0  = occursNever unit  num && 
                occursNever unit  den
  | exp == 1  = occursOnce  unit  num &&
                occursNever unit' num && 
                occursNever unit  den
  | exp == -1 = occursOnce  unit  den &&
                occursNever unit  num
  | exp > 1   = occursOnce  unit' num &&
                occursNever unit  den
  | exp < -1  = occursOnce  unit' den &&
                occursNever unit  num
  | otherwise = True
  where
    num = takeWhile(/='/') str
    den = dropWhile(/='/') str
    unit' = unit ++ "^" ++ (show (abs exp))

-- TODO: Check that it only occurs once.

-- kg^2 (eller dylikt) ska finns exakt en gång i rätt nivå
-- och inte finns alls i fel nivå.

-- Exatcly once. Not more or less
occursOnce :: (Eq a) => [a] -> [a] -> Bool
occursOnce a = (==1) . numOccurs a

occursNever :: (Eq a) => [a] -> [a] -> Bool
occursNever a = (==0) . numOccurs a

-- How many times the first list occurs in the second one
numOccurs :: (Eq a) => [a] -> [a] -> Int
numOccurs subList list = len $ filter (==subList) sg
  where
    sg = subGroups (len subList) list

-- Groups the list into lists of the specified length
subGroups :: Int -> [a] -> [[a]]
subGroups n _
  | n < 1 = error "Groups must be at least 1 big"
subGroups _ [] = []
subGroups n list@(_:rest)
  | n > len list = []
  | otherwise    = (take n list):(subGroups n rest)

-- Length of a list
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs
