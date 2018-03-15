
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Dimensions.Quantity.Test2metarun
(
)
where

import Prelude hiding (length, div)
import Test.QuickCheck
import Language.Haskell.TH

import Dimensions.TypeLevel
import Dimensions.Quantity

import Dimensions.Quantity.Test2

------------------------------------------------------------

