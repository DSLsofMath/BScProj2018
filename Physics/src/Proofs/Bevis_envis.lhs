
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeInType #-}

> module Proofs.Bevis_envis
> (
> )
> where

En typ av sort Equal x y är ett bevis och ett värde är då beviset

Men det som är Equal mellan är uttryck. Detta behöver man skilja åt, så kanske ha sort för uttryck och sort för bevis?

Används som typ

> data Expr = Div Expr Expr
>           | Mul Expr Expr
>           | A
>           | V
>           | T

> data Equal (a :: Expr) (b :: Expr) where
>   Refl :: Equal c c
>   Avg :: Equal A (Div V T)

> x = Refl
> y = Avg
> z = Refl :: (Equal V V)

"Ekvivalens" mellan två uttryck ska inte gå. Behöver kanske ha sort för bevis. Bevis här är bara att likheter gäller.

> -- Ett försök att lösa ovanstående
> --data Eqvi (a :: Equal (x :: Expr) (y :: Expr)) (b :: Equal (p :: Expr) (q :: Expr)) where
> --  Self :: Eqvi c c -- x implies x
> --  MulUpDiv :: Eqvi (Equal a (Div b c)) (Equal (Mul a c) b)

> data Eqvi a b where
>   Self :: Eqvi c c -- x implies x
>   MulUpDiv :: Eqvi (Equal a (Div b c)) (Equal (Mul a c) b)

> -- Tar en ekivalens, och dessa ena premiss, 
> -- och skapar dess konsekvens
> transform :: a -> Eqvi a b -> b
> transform = undefined


> reflexive :: Equal a a
> reflexive = Refl

> symetric :: Equal a b -> Equal b a
> symetric Refl = Refl
> -- ?

> transitive :: Equal a b -> Equal b c -> Equal a c
> transitive Refl Refl = Refl
> -- ?


> -- Axiom
> s0 :: Equal A (Div V T)
> s0 = Avg

> s1 :: Equal (Mul A T) V
> s1 = transform s0 MulUpDiv