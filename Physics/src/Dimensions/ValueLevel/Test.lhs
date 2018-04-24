
Testing of value-level dimensions
=================================

![Both you, me and dimensions need to obey the laws](Laws.png){.float-img-left}

For operations on dimensions, there are a number of laws which should hold. We will here test that the value-level dimensions obey them. One way is to use `QuickCheck`, which produces lots o' random test cases.

> module Dimensions.ValueLevel.Test
>     ( runTests
>     ) where

> import Prelude hiding (length, div)
> import Test.QuickCheck
> import Data.List

> import Dimensions.ValueLevel

Generating arbitrary dimensions
-------------------------------

The first thing one needs in order to use `QuickCheck` is an instance of `Arbitrary` for the data type to be used in the tests. In this case it's `Dim`.

An arbitrary example of an `Arbitrary` instance (get it?) could look like

< data IntPair = IntPair (Int, Int)

< genIntPair :: Gen IntPair
< genIntPair = do
<   first  <- arbitrary
<   second <- arbitrary
<   return $ IntPair (first, second)

< instance Arbitrary IntPair where
<   arbitrary = genIntPair

**Exercise.** Now try to implement an `Arbitrary` instance of `Dim`.

<details>
<summary>**Solution**</summary>
<div>

Here's one way to do it.

> genDim :: Gen Dim
> genDim = do
>   le <- arbitrary
>   ma <- arbitrary
>   ti <- arbitrary
>   cu <- arbitrary
>   te <- arbitrary
>   su <- arbitrary
>   lu <- arbitrary
>   return (Dim le ma ti cu te su lu)

> instance Arbitrary Dim where
>   arbitrary = genDim

</div>
</details>

Properties for operations on dimensions
---------------------------------------

Since dimensions are treated just like regular numbers when it comes to multiplication and division, the laws which ought to hold should be pretty clear. It's the "obvious" laws such as commutativity and so on.

The laws to test are

- Multiplication is commutative
- Multiplication is associative
- `one` is a unit for multiplication
- Multiplication and division cancel each other out
- Dividing by `one` does nothing
- Dividing by a division brings up the lowest denominator [TODO: strange wording?? Perhaps use a formula instead. (and perhaps use `recip` to simplify?)]
- Multiplication by $x$ is the same as dividing by the inverse of $x$.

The implementation of the first law looks like

> -- Property: multiplication is commutative
> prop_mulCommutative :: Dim -> Dim -> Bool
> prop_mulCommutative d1 d2 = d1 `mul` d2 == d2 `mul` d1

**Excercise.** Implement the rest.

<details>
<summary>**Solution**</summary>
<div>

Here's what the rest could look like.

> -- Property: multiplication is associative
> prop_mulAssociative :: Dim -> Dim -> Dim -> Bool
> prop_mulAssociative d1 d2 d3 = d1 `mul` (d2 `mul` d3) ==
>  (d1 `mul` d2) `mul` d3

> -- Property: `one` is a unit for multiplication
> prop_mulOneUnit :: Dim -> Bool
> prop_mulOneUnit d = d == one `mul` d

> -- Property: dividing twice results in no change
> prop_divTwice :: Dim -> Dim -> Bool
> prop_divTwice d1 d2 = d1 `div` (d1 `div` d2) == d2

> -- Property: multiplication and division cancel each other out
> prop_mulDivCancel :: Dim -> Dim -> Bool
> prop_mulDivCancel d1 d2 = (d1 `mul` d2) `div` d1 == d2

> -- Property: dividing by `one` does noting
> prop_divOne :: Dim -> Bool
> prop_divOne d = d `div` one == d

> -- Property: multiplication same as division by inverse
> prop_mulDivInv :: Dim -> Dim -> Bool
> prop_mulDivInv d1 d2 = d1 `mul` d2 ==
>   d1 `div` (one `div` d2)

</div>
</details>

Testing the pretty-printer
--------------------------

TODO: It is good that you test it, but I suggest you keep that out of the learning material [perhaps a link to "extra reading"].

We rely pretty heavily on the pretty-printer. Let's test it too! We won't get too ambitious in our testing of it. It'll be enough to check that if a dimension has a nonzero

TODO: Do something about the problem of m and mol.

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

Integrating tests with Stack
----------------------------

This project uses `Stack`. One part of `Stack` is continous testing, and for it to work on the tests we developed here, the following functions is needed.

> runTests :: IO ()
> runTests = do
>   putStrLn "Dimensions value-level: Multiplication commutative"
>   quickCheck prop_mulCommutative
>   putStrLn "Dimensions value-level: ;ultiplication associative"
>   quickCheck prop_mulAssociative
>   putStrLn "Dimensions value-level: `one` is unit for multiplication"
>   quickCheck prop_mulOneUnit
>   putStrLn "Dimensions value-level: Dividing by a division brings up the lowest denominator"
>   quickCheck prop_divTwice
>   putStrLn "Dimensions value-level: Multiplication and divison cancel each other out"
>   quickCheck prop_mulDivCancel
>   putStrLn "Dimensions value-level: Dividing by `one` does nothing"
>   quickCheck prop_divOne
>   putStrLn "Dimensions value-level: Multiplication by x is the same as dividing by the inverse of x"
>   quickCheck prop_mulDivInv
