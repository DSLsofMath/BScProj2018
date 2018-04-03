
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Proofs.Test where

import Proofs.Proofs

t :: Ti `Equal` T0
t = Tinitial

hej :: (Avalue `Mul` (Tf `Sub` Ti)) `Equal` 
       (Avalue `Mul` (Tf `Sub` T0))
hej = undefined

skoj :: (a `Mul` (b `Sub` Ti)) `Equal`
        (a `Mul` (b `Sub` T0))
skoj = Cong x
  where
    x :: (b `Sub` Ti) `Equal` (b `Sub` T0)
    x = Cong y
    
    y :: Ti `Equal` T0
    y = t

ark :: (Avalue `Mul` (Ti `Sub` Tf)) `Equal` 
       (Avalue `Mul` (T0 `Sub` Tf))
ark = Cong x
  where
    x :: (Ti `Sub` Tf) `Equal` (T0 `Sub` Tf)
    x = congSub t

congSub :: a `Equal` b -> (a `Sub` c) `Equal` (b `Sub` c)
congSub = undefined





































