
-- Ett försök att lösa integral-fusk-problemet

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Proofs.New where

-- Tid är alltid för sig och annat är alltid för sig.
-- Därför behövs en funktion t(t)

{-
----------------------------- En typ av grej
|       |       |      |
|       |       |      |
v(t) = v_i + a_value * t(t)
  |                      |
  |                      |
  ---------------------------- En annan typ av grej
-}

-- Function Expression
data FE = C OC    -- En funktion kan vara (en) konstant
        | FE `Add` FE  -- En summa av två andra funktioner
        | FE `Mul` FE  -- En produkt av två andra funktioner
        | FE `Sub` FE
        | Tfun TC   -- t(t) evaluerad i nån tid
        | Xfun TC   -- x(t) evaluerad i nån tid
        | Vfun TC   -- v(t) evaluerad i nån tid
        | DeltaVfun TC 
        | I FE TC TC -- En integral av nån funktion

-- Time Constant
data TC = T0
        | Ti
        | Tf

-- Other Constant
data OC = X0
        | Xi
        | Xf
        | V0
        | Vi
        | Vf
        | Avalue

data Equal (x :: FE) (y :: FE) where
  Xinitial :: Equal (C X0) (C Xi)
  Vinitial :: Equal (C V0) (C Vi)
  DeltaVdef :: Equal (DeltaVfun t) (Vfun t `Sub` Vfun Ti)
  IntegConst :: Equal (I (C c) u l) ((C c) `Mul` (Tfun u `Sub` Tfun l))
  
-- När det gäller Integ så gör en generell för polynom av grad 1, sedan bevisas specifika.

































