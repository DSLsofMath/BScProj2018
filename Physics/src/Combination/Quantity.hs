
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Combination.Quantity where

import qualified Dimensions.ValueLevel as V
import           Dimensions.TypeLevel  as T
import           Prelude               as P hiding (length)

----------------------------------------
-- Än så länge inget nytt
----------------------------------------

data Quantity (d :: T.Dim) (v :: *) where
  ValQuantity :: V.Dim -> v -> Quantity d v

showQuantity :: (Show v) => Quantity d v -> String
showQuantity (ValQuantity d v) = show v ++ " " ++ show d

instance (Show v) => Show (Quantity d v) where
  show = showQuantity

instance (Eq v) => Eq (Quantity d v) where
  (ValQuantity _ v1) == (ValQuantity _ v2) = v1 == v2

instance (Ord v) => Ord (Quantity d v) where
  (ValQuantity _ v1) `compare` (ValQuantity _ v2) = v1 `compare` v2

instance Functor (Quantity d) where
  fmap f (ValQuantity d v) = ValQuantity d (f v)

----------------------------------------
-- Socker
----------------------------------------

infixl 3 ##
(##) :: v -> Quantity d w -> Quantity d v
v ## (ValQuantity d _) = ValQuantity d v

-- Dummy-värden med matchande värde/typ-nivå dimensioner
-- med en dummy-typ.

length :: Quantity Length Double
length = ValQuantity V.length 1.0
mass :: Quantity Mass Double
mass = ValQuantity V.mass 1.0
time :: Quantity Time Double
time = ValQuantity V.time 1.0
one :: Quantity One Double
one = ValQuantity V.one 1.0

-- Med `##` kan en Quantity med vilken värdetyp som helst skapas
-- med valfri dimension av ovanstående.

-- Om värdetypen ej stöder multiplikation och division kan
-- dessa dummy-värden ändå göras så på, och därför kan man
-- alltid få valfri dimension.

----------------------------------------
-- Aritmetik
----------------------------------------

-- En `Quantity` innehåller något av någon typ. Om och hur addition
-- o.s.v. ser ut för den kan variera, så typen själv ska sköta det.
-- Dessutom kan det var multiplikation mellan olika typer.

class Addable a b c where
  doAdd :: a -> b -> c

(+#) :: (Addable a b c) => Quantity d a ->
                           Quantity d b ->
                           Quantity d c
(ValQuantity d a) +# (ValQuantity _ b) = ValQuantity d $ doAdd a b

-- Nedan går ej! Blir problem med Vector då. Vet ej varför.

-- Allt "numeriskt" är adderbart
--instance (Num v) => Addable v v v where
--  doAdd = (+)

----------

class Subable a b c where
  doSub :: a -> b -> c

(-#) :: (Subable a b c) => Quantity d a ->
                           Quantity d b ->
                           Quantity d c
(ValQuantity d a) -# (ValQuantity _ b) = ValQuantity d $ doSub a b

--instance (Num v) => Subable v v v where
--  doSub = (-)

----------

class Multiplicable a b c where
  doMul :: a -> b -> c

(*#) :: (Multiplicable a b c) => Quantity d1 a ->
                                 Quantity d2 b ->
                                 Quantity (d1 `Mul` d2) c
(ValQuantity d1 a) *# (ValQuantity d2 b) = ValQuantity (d1 `V.mul` d2) $ doMul a b

--instance (Num v) => Multiplicable v v v where
--  doMul = (*)

----------

class Divisionable a b c where
  doDiv :: a -> b -> c

(/#) :: (Divisionable a b c) => Quantity d1 a ->
                                Quantity d2 b ->
                                Quantity (d1 `Div` d2) c
(ValQuantity d1 a) /# (ValQuantity d2 b) = ValQuantity (d1 `V.div` d2) $ doDiv a b

--instance (Fractional v) => Divisionable v v v where
--  doDiv = (/)

----------------------------------------
-- Derivering och integrering
----------------------------------------

-- Är själva grejen som finns i en Quantity deriverbar och
-- integrerbar ska Quantityn med den i också vara det.

class Calculable v where
  doDif :: v -> v
  doInteg :: v -> v

diff :: (Calculable v) => Quantity d v -> Quantity (d `Div` Time) v
diff (ValQuantity d v) = ValQuantity (d `V.div` V.time) $ doDif v

-- Inte det snyggaste...

integ :: (Calculable v) => Quantity d v -> Quantity (d `Mul` Time) v
integ (ValQuantity d v) = ValQuantity (d `V.mul` V.time) $ doInteg v

----------------------------------------
-- Hack
----------------------------------------

-- Eftersom det blir problem med Num som instans av många
-- görs här manuellt för vissa datatyper

instance Addable Double Double Double where
  doAdd = (+)

instance Subable Double Double Double where
  doSub = (-)

instance Multiplicable Double Double Double where
  doMul = (*)

instance Divisionable Double Double Double where
  doDiv = (/)

































































