{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Dimensions.Quantity2 where

import           Dimensions.TypeLevel  as T
import qualified Dimensions.ValueLevel as V
import           Prelude               as P hiding (length)

data Quantity (d :: T.Dim) (v :: *) where
  ValQuantity :: V.Dim -> v -> Quantity d v

showQuantity :: (Show v) => Quantity d v -> String
showQuantity (ValQuantity d v) = show v ++ " " ++ show d

instance (Show v) => Show (Quantity d v) where
  show = showQuantity

----------------------------------

class Addable a b c where
  doAdd :: a -> b -> c

(+#)        :: (Addable a b c) => Quantity d a ->
                                  Quantity d b ->
                                  Quantity d c
(+#)         (ValQuantity d a) (ValQuantity _ b) =
             ValQuantity d $ doAdd a b

--instance (Num v) => Addable v v v where
--  doAdd = (+)

instance Addable Double Double Double where
  doAdd = (+)

----------------------------------

class Multiplicable a b c where
  doMult :: a -> b -> c

(*#)         :: (Multiplicable a b c) => Quantity d1 a
                                      -> Quantity d2 b
                                      -> Quantity (d1 `Mul` d2) c
(*#)         (ValQuantity d1 a) (ValQuantity d2 b) =
             ValQuantity (d1 `V.mul` d2) $ doMult a b


instance (Num v) => Multiplicable v v v where
  doMult = (*)

-- instance (Num v) => Multiplicable Double Double Double where
--   doMult = (*)







class Creatable a where
  anyVal :: a

-- Vad går att "skapa"?

instance Creatable Double where
  anyVal = 1.0

instance Creatable Integer where
  anyVal = 1

--instance (Num v) => Creatable v where
--  anyVal = fromInteger 0
-- Det blir problem om denna finns med. Overlapping instances.
-- Tas den i combination bort, så klagas det att Num för Vector2 Double krävs. Läggs en sådan till (utan någon definerat) funkar det.

infixl 3 ##
(##) :: v -> Quantity d v -> Quantity d v
v ## (ValQuantity d _) = ValQuantity d v

length :: (Creatable v) => Quantity Length v
length = ValQuantity V.length anyVal
mass :: (Creatable v) => Quantity Mass v
mass = ValQuantity V.mass anyVal
time :: (Creatable v) => Quantity Time v
time = ValQuantity V.time anyVal


















































