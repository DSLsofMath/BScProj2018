module Vec2 where

import           Calculus.Calculus     as C
import           Dimensions.Quantity   as Q
import           Dimensions.TypeLevel  as T
import           Dimensions.ValueLevel as V
-- import Vector.Vector         as Vec

class Vector vector where
  vmap      :: (Expr -> Expr) -> vector  -> vector
  vzipWith  :: (Expr -> Expr  -> Expr) -> vector -> vector -> vector
  vfold     :: (Expr -> Expr  -> Expr) -> vector -> Expr
  -- transform :: vector -> vector

--data Vector1 = V1 Expr
data Vector2 = V2 Expr Expr
data Vector3 = V3 Expr Expr Expr

instance Vector Vector2 where
  vmap f (V2 x y)                  = V2 (f x) (f y)
  vzipWith f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)
  vfold f (V2 x y)                 = f x y

add :: Vector v => v -> v -> v
add v1 v2 = vzipWith (+) v1 v2

magnitude :: Vector vec => vec -> Expr
magnitude v = sqrt . vfold (+) $ vmap (**2) v

dotProduct :: Vector v => v -> v -> Expr
dotProduct v1 v2 = vfold (*) $ vzipWith (+) v1 v2

