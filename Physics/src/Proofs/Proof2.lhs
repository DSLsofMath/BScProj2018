
WIP

The second proof
================

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}

> module Proofs.Proof2
> (
> )
> where

> import Proofs.Kinematics
> import Proofs.Proof1


Vi ska nu bevisa

\begin{align}
  x(t) = x_i + v_i * t + \frac{a_{värde}*t^2}{2}
\end{align}

**(I)**

Vi börjar där precis som innan med ett axiom

> dxtEinv :: DeltaXfun t `Equal` Integ (Vfun t') Ti t t'
> dxtEinv = DeltaXint

\begin{align}
  (\Delta x)(t) = \int_{t_i}^t v(t) dt
\end{align}

**(II)**

Eftersom integraler är inblandade behöver vi omvandla `Vfun t` till ett polynom.

> vfun :: Vfun t `Equal` ((Avalue `Mul` t) `Add` Vi)
> vfun = vFunEavalMtAvi

> vfun' :: Vfun t `Equal` ((Vi `Add` (Avalue `Mul` t)) `Add` 
>                         (Zero `Mul` (t `Mul` t)))
> vfun' = vfun `Transitivity` x
>   where
>     x :: ((Avalue `Mul` t) `Add` Vi) `Equal` 
>          ((Vi `Add` (Avalue `Mul` t)) `Add` 
>          (Zero `Mul` (t `Mul` t)))
>     x = Symmetry s
>     y :: ((Avalue `Mul` t) `Add` Vi) `Equal` 
>          ((Avalue `Mul` t) `Add` Vi)
>     y = Reflexive
>     z :: ((Avalue `Mul` t) `Add` Vi) `Equal` 
>          (Vi `Add` (Avalue `Mul` t))
>     z = y `Transitivity` AddCom
>     q :: (((Avalue `Mul` t) `Add` Vi) `Add` 
>             (Zero `Mul` (t `Mul` t))) `Equal` 
>          ((Vi `Add` (Avalue `Mul` t)) `Add` 
>             (Zero `Mul` (t `Mul` t)))
>     q = CongAddL z
>     r :: ((Vi `Add` (Avalue `Mul` t)) `Add` 
>           (Zero `Mul` (t `Mul` t))) `Equal` 
>          (((Avalue `Mul` t) `Add` Vi) `Add` Zero) 
>     r = (Symmetry q) `Transitivity` (CongAddR ZeroMul)
>     s :: ((Vi `Add` (Avalue `Mul` t)) `Add` 
>           (Zero `Mul` (t `Mul` t))) `Equal` 
>          ((Avalue `Mul` t) `Add` Vi) 
>     s = r `Transitivity` ZeroAddR

Nu kan vi göra till en polynomfunktion.

> vfunPoly :: Vfun t `Equal` PolyFun Vi Avalue Zero t
> vfunPoly = vfun' `Transitivity` (Symmetry PolyEval)

**(III)**

Vi återvänder till det första steget och omvandlar till polynomfunktion

> dxtEinvfp :: DeltaXfun t `Equal` Integ (PolyFun Vi Avalue Zero t') Ti t t'
> dxtEinvfp = dxtEinv `Transitivity` CongInteg vfunPoly

**(IV)**

Integralen beräknas

> dxtEpSp :: (DeltaXfun t) `Equal` 
>         ((PolyFun Zero Vi (Avalue `Div` Two) t) `Sub`
>         (PolyFun Zero Vi (Avalue `Div` Two) Ti))
> dxtEpSp = dxtEinvfp `Transitivity` IntegEval

**(V)**

Vi gör ett lemma för att omvandla polynomfunktionen

> polyCubeLin :: PolyFun Zero a1 a2 t `Equal` 
>                ((a1 `Mul` t) `Add` (a2 `Mul` (t `Mul` t)))
> polyCubeLin = x `Transitivity` y
>   where
>     x :: PolyFun Zero a1 a2 t `Equal` 
>          ((Zero `Add` (a1 `Mul` t)) `Add` (a2 `Mul` (t `Mul` t)))
>     x = PolyEval
>     y :: ((Zero `Add` (a1 `Mul` t)) `Add` 
>           (a2 `Mul` (t `Mul` t))) `Equal` 
>          ((a1 `Mul` t) `Add` (a2 `Mul` (t `Mul` t)))
>     y = CongAddL ZeroAddL

och så använder vi den

> -- Cubic Linear
> type CL a1 a2 t = (a1 `Mul` t) `Add` (a2 `Mul` (t `Mul` t))
> -- Avalue Half
> type AH = Avalue `Div` Two

> s5 :: DeltaXfun t `Equal`
>       (CL Vi AH t
>      -- (Add (Mul Vi t) (Mul (Div Avalue Two) (Mul t t)))
>           `Sub`
>        CL Vi AH Ti)
>      -- (Add (Mul Vi Ti) (Mul (Div Avalue Two) (Mul Ti Ti))))
> s5 = y
>   where
>     x = dxtEpSp `Transitivity` CongSubL polyCubeLin
>     y = x `Transitivity` CongSubR polyCubeLin

\begin{align}
  (\Delta x)(t) = v_i * t + \frac{a_{value}*t^2}{2} - (v_i * t_i + \frac{a_{value}*t_i^2}{2})
\end{align}

**(VI)**

Vi vill nyttja konventionen $t_i = 0$ igen nu.

> toZero :: CL a1 a2 Ti `Equal` Zero
> toZero = u
>   where
>     x :: ((a1 `Mul` Ti) `Add` (a2 `Mul` (Ti `Mul` Ti))) `Equal`
>          ((a1 `Mul` Ti) `Add` (a2 `Mul` (Ti `Mul` Ti)))
>     x = Reflexive
>     y :: (Zero `Add` (a2 `Mul` (Ti `Mul` Ti))) `Equal`
>          ((a1 `Mul` Ti)   `Add` (a2 `Mul` (Ti `Mul` Ti)))
>     y = CongAddL p
>     z :: (c `Mul` Zero) `Equal` (c `Mul` Ti)
>     z = CongMulR (Symmetry Tinitial2)
>     p :: Zero `Equal` (c `Mul` Ti)
>     p = Symmetry ZeroMulR `Transitivity` z
>
>     q :: (a2 `Mul` (Ti `Mul` Ti)) `Equal` (a2 `Mul` (Ti `Mul` Zero))
>     q = CongMulR (CongMulR Tinitial2)
>     
>     r = (CongMulR ZeroMulR)
>     s = (q `Transitivity` r) `Transitivity` ZeroMulR
>     t = Symmetry y `Transitivity` CongAddR s
>     u = t `Transitivity` ZeroAddL

> s6 :: DeltaXfun t `Equal` CL Vi AH t
> s6 = (s5 `Transitivity` CongSubR toZero) `Transitivity` ZeroSub

\begin{align}
  (\Delta x)(t) = v_i * t + \frac{a*t^2}{2}
\end{align}

































