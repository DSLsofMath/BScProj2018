
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Combination.MWE where

class Addable a b c where
  doAdd :: a -> b -> c

addera :: (Addable a b c) => a -> b -> c
addera = doAdd

--instance (Addable Double Double Double) where
--  doAdd = (+)

instance (Num v) => Addable v v v where
  doAdd = (+)

data Container (d :: *) (v :: *) where
  ValContainer :: v -> Container d v

addCon :: (Addable v v v) => Container d v -> Container d v -> Container d v
addCon (ValContainer x) (ValContainer y) = ValContainer $ doAdd x y

v1 :: Container Bool Double
v1 = ValContainer 2.0
v2 :: Container Bool Double
v2 = ValContainer 3.0

containerAdd :: (Addable a b c) => Container d a ->
                                   Container d b ->
                                   Container d c
containerAdd (ValContainer a) (ValContainer b) = ValContainer (doAdd a b)

v3 :: Container Bool Double
v3 = containerAdd v1 v2
















