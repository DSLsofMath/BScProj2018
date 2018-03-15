
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Dimensions.Quantity.Test2 where

import Prelude hiding (length, div)
import Test.QuickCheck
import Language.Haskell.TH

import Dimensions.TypeLevel
import Dimensions.Quantity

------------------------------------------------------------


genQuantityD :: Quantity d Rational -> Gen (Quantity d Rational)
genQuantityD (Quantity d _) = do
  v <- arbitrary
  return (Quantity d v)

f1 = genQuantityD mass
f2 = genQuantityD time

instance Arbitrary (Quantity Mass Rational) where
  arbitrary = f1

instance Arbitrary (Quantity Time Rational) where
  arbitrary = f2

prop_mulComTT :: Quantity Time Rational -> Quantity Time Rational -> Bool
prop_mulComTT q1 q2 = q1 *# q2 == q2 *# q1

prop_mulComTM :: Quantity Time Rational -> Quantity Mass Rational -> Bool
prop_mulComTM q1 q2 = q1 *# q2 == q2 *# q1

-- Detta ska nu generaliseras med template Haskell

--propTH :: Quantity d1 Rational -> Quantity d2 Rational -> Q Exp
--propTH q1 q2 = return $ VarE _

hej :: Int -> Q [exp]
hej n = do
  