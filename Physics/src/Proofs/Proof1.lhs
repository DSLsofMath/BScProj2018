
WIP

The first proof
===============

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}

> module Proofs.Proof1
> ( 
> )
> where

> import Proofs.Kinematics

The goal of this chapter is to prove the following formula

\begin{align}
  v_f = v_i + a*t 
\end{align}

which more precisely should be written as

\begin{align}
  v_f = v_i + a_{value} * (\Delta t)(t_f)
\end{align}

---

**Exercise.**

Write the type of the statement we want to prove.

**Solution.**

< Vf `Equal` (Vi `Add` (Avalue `Mul` DeltaTfun Tf))

---

Let's go!

**(I)**

Let's begin by having two of the defining equalities here

> afEav :: Afun t `Equal` Avalue
> afEav = AfunCon

> dvEiaf :: DeltaVfun t `Equal` Integ (Afun t') Ti t t'
> dvEiaf = DeltaVint

\begin{align}
  a(t) = a_{value} && (\Delta v)(t) = \int_{t_i}^t a(t') dt'
\end{align}

We'll give values "mnemonic" names. `afEav` is composed of

- `af` - acceleration function
- `E` - equals
- `av` - acceleration value

We won't explain all these. Hopefully they make sense.

**(II)**

We replace (the general) `Afun t'` with its actual value `Avalue`.

> dvEiav :: DeltaVfun t `Equal` Integ Avalue Ti t t'
> dvEiav = y
>   where
>     x :: Integ (Afun t') x y z `Equal` Integ Avalue x y z
>     x = CongInteg afEav

`x` is an equality stating the any integral matching the left-hand-side can be replaced with the right-hand-side one. The one in `dvEiaf` does, so we use `Transitivity` to replace it.

>     y :: DeltaVfun t `Equal` Integ Avalue Ti t t'
>     y = dvEiaf `Transitivity` x

A general trick with `Transitivity` is to think *what you want to replace* and *what you want to keep*. Here, we wanted to *keep* `DeltaVfun t`. So it should have the role of `a` in

< Transitivity  :: a `Equal` b -> b `Equal` c -> a `Equal` c

and hence the equality having what you want to keep on the left-hand-side, should be the left argument to `Transitivity`. The right argument should have on the left-hand-side of its equaility a type matching the one you wanted to replace in the first equality.

\begin{align}
  (\Delta v)(t) = \int_{t_i}^t a_{value} dt'  
\end{align}

**(III)**

We want to evaluate the integral. We have one axiom for that

< IntegEval :: Integ (PolyFun a0 a1 Zero t) l u t
<                `Equal`
<              ((PolyFun Zero a0 (a1 `Div` Two) u) 
<                  `Sub`
<               (PolyFun Zero a0 (a1 `Div` Two) l))

but it requires the integrand to be a `PolyFun`. In other words, we need to transform `Avalue` (our current integrand) to a `PolyFun` of some sort. We have an axiom for that as well

< PolyEval :: PolyFun a0 a1 a2 t `Equal` ((a0 `Add` (a1 `Mul` t)) `Add` (a2 `Mul` (t `Mul` t)))


TODO: Stopp här. Det innan måste göras om. a0 kan sättas till Avalue `Mul` t och då kan man fuska ändå! buhuhu








> {-

Vi beräknar integralen

> dvEavaltSavalti :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue (Zero `Div` Two) t) 
>         `Sub` 
>        (PolyFun Zero Avalue (Zero `Div` Two) Ti))
> dvEavaltSavalti = dvEinaval `Transitivity` IntegEval

\begin{align}
  (\Delta v)(t) = a_{value} * t - a_{value} * t_i
\end{align}

**(IV)**

Vi ska hyfsa den där likheten lite. Semantiskt blir det dock ingen skillnad från steget innan. Först görs `Zero \`Div\` Two` till `Zero`.

> dvEavaltSavalti1 :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue Zero t) 
>         `Sub` 
>        (PolyFun Zero Avalue (Zero `Div` Two) Ti))
> dvEavaltSavalti1 = dvEavaltSavalti `Transitivity` x
>   where
>     x = CongSubL y
>     y = CongPoly2 ZeroNum

och den andra av dem

> dvEavaltSavalti2 :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue Zero t) 
>         `Sub` 
>        (PolyFun Zero Avalue Zero Ti))
> dvEavaltSavalti2 = dvEavaltSavalti1 `Transitivity` x
>   where
>     x ::  (PolyFun Zero Avalue Zero t `Sub` 
>            PolyFun Zero Avalue (Div Zero Two) Ti)
>          `Equal`
>           (PolyFun Zero Avalue Zero t `Sub` 
>            PolyFun Zero Avalue Zero           Ti)
>     x = CongSubR y
>     y = CongPoly2 ZeroNum

**(V)**

Det är ganska uppenbart för oss människor att

\begin{align}
  0 + a*t + 0*t*t = a*t
\end{align}

men i datorn måste vi ändå göra det fullständigt. Vi inser att det kommer behöva göras flera gånger och vara väldigt omständigt. Så vi gör en liten generell lemma.

> polyLinear :: PolyFun Zero e Zero t `Equal` (e `Mul` t)
> polyLinear = p
>   where
>     x :: PolyFun Zero e Zero t `Equal` 
>          ((Zero `Add` (e `Mul` t)) `Add` (Zero `Mul` (t `Mul` t)))
>     x = PolyEval
>
>     y :: PolyFun Zero e Zero t `Equal` 
>          ((Zero `Add` (e `Mul` t)) `Add` Zero)
>     y = x `Transitivity` (CongAddR ZeroMul)
>
>     z :: PolyFun Zero e Zero t `Equal` (Zero `Add` (e `Mul` t))
>     z = y `Transitivity` ZeroAddR
>
>     p :: PolyFun Zero e Zero t `Equal` (e `Mul` t)
>     p = z `Transitivity` ZeroAddL

Så... vad ska detta användas på? Jo den ska användas för att förbättra hygienen från det föregående steget.

> dvEavaltSavalti3 :: DeltaVfun t `Equal` ((Avalue `Mul` t) `Sub`
>                                          (Avalue `Mul` Ti))
> dvEavaltSavalti3 = y
>   where
>     x = dvEavaltSavalti2 `Transitivity` CongSubL polyLinear
>     y = x                `Transitivity` CongSubR polyLinear

**(VI)**

Nu är vi nästan klara.

> dvEavalMtSti :: DeltaVfun t `Equal` (Avalue `Mul` (t `Sub` Ti))
> dvEavalMtSti = dvEavaltSavalti3 `Transitivity` 
>                (Symmetry MulDistSub)

\begin{align}
  (\Delta v)(t) = a_{value} * (t - t_i)
\end{align}

**(VII)**

Vi vecklar upp vänsterledet

> vfunSviEavalMtSti :: (Vfun t `Sub` Vi) `Equal` 
>                      (Avalue `Mul` (t `Sub` Ti))
> vfunSviEavalMtSti = (Symmetry DeltaVdef) `Transitivity`
>                     dvEavalMtSti

och flyttar över `Vi`

...

Ah! Vi har inget axiom för det. Vi kan bara addera `Vi` på båda sidor och sedan kancellera det på VL. Låt oss bevisa ett lemma.

> subToAdd :: (a `Sub` b) `Equal` c -> a `Equal` (c `Add` b)
> subToAdd aSbEc = y
>   where
>     --x :: ((a `Sub` b) `Add` b) `Equal` (c `Add` b)
>     x = CongAddL aSbEc
>     y = (Symmetry AddSub) `Transitivity` x

Vi nyttjar lemmat

> vfunEavalMtStiAvi :: Vfun t `Equal` ((Avalue `Mul` (t `Sub` Ti)) `Add` Vi)
> vfunEavalMtStiAvi = subToAdd vfunSviEavalMtSti

\begin{align}
  v(t) = a_{value} * (t - t_i) + v_i
\end{align}

**(VIII)**

Vi använder konventionen $t_i = 0$.

> vFunEavalMtAvi :: Vfun t `Equal` ((Avalue `Mul` t) `Add` Vi)
> vFunEavalMtAvi = vfunEavalMtStiAvi `Transitivity` x
>   where
>     x :: ((Avalue `Mul` (Sub t Ti)) `Add` Vi) `Equal` 
>          ((Avalue `Mul` t         ) `Add` Vi)
>     x = CongAddL y
>     y :: (Avalue `Mul` (t `Sub` Ti)) `Equal` 
>          (Avalue `Mul` t           )
>     y = CongMulR z
>     z :: (t `Sub` Ti) `Equal` t
>     z = r `Transitivity` ZeroSub
>     q :: (t `Sub` Ti) `Equal` (t `Sub` Ti)
>     q = Reflexive
>     r :: (t `Sub` Ti) `Equal` (t `Sub` Zero)
>     r = q `Transitivity` CongSubR Tinitial2

\begin{align}
  v(t) = a_{value} * t + v_i
\end{align}

Phew! Så där. Nu kan vi känna oss nöja med detta första bevis.

> -}































