
> {-# LANGUAGE TypeOperators #-}

> module Dimensions.Usage where

> import Dimensions.Quantity
> import Dimensions.TypeLevel
> import Prelude hiding (length)

> areaOfRect :: Quantity Length Double -> Quantity Length Double -> Quantity (Length `Mul` Length) Double
> areaOfRect width height = width *# height