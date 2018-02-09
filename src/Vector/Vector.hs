module Vector where

type Scalar = Double
type Angle  = Double

class Vector vec where
  vmap      :: (Scalar -> Scalar) -> vec -> vec
  vzipWith  :: (Scalar -> Scalar  -> Scalar) -> vec -> vec -> vec
  vfold     :: (Scalar -> Scalar  -> Scalar) -> vec -> Scalar

dotProd :: Vector vec => vec -> vec -> Scalar
dotProd v1 v2 = vfold (*) $ vzipWith (+) v1 v2

magnitude :: Vector vec => vec -> Scalar
magnitude v = sqrt . vfold (+) $ vmap (**2) v

data Vector3 = V3 Scalar Scalar Scalar
data Vector2 = V2 Scalar Scalar

lift :: Vector2 -> Vector3
lift (V2 x y) = V3 x y 0

zeroVector :: (Vector a, Num a) => a
zeroVector = 0


crossProd :: Vector3 -> Vector3 -> Vector3
crossProd (V3 x y z) (V3 x' y' z') = V3 (y*z' - z*y') -- X
                                        (z*x' - x*z') -- Y
                                        (x*y' - y*x') -- Z

prop_crossProd :: Vector3 -> Bool
prop_crossProd v = (crossProd v v) == 0

-- The angle 
angle :: Vector2 -> Scalar
angle (V2 x y) = atan y/x

-- Angle between two vectors
angleG :: Vector vec => vec -> vec -> Scalar
angleG v1 v2 = (dotProd v1 v2) / ((magnitude v1) * (magnitude v2))

mkVector :: Scalar -> Angle -> Vector2
mkVector mag angle = V2 x y
  where
    x = mag * cos angle
    y = mag * sin angle

v1 = V2 1 2
v2 = V2 3 4

-- | TODO
-- | ~Laws~
-- | Langrange's formula: a x (b x c) = b(a * c) - c(a * b)
-- | Cross product is anticommutative
-- | Jacobi identity: a x (b x c) + b x (c x a) + c x (a x b) = 0

-- | Unit vectors
-- | Cross product
-- | Angles

-- | DONE
-- | Associativity of addition follows from Monoid instance!

instance Monoid Vector2 where
  mempty  = zeroVector
  mappend = (+)
  mconcat = foldr mappend mempty

instance Monoid Vector3 where
  mempty  = zeroVector
  mappend = (+)
  mconcat = foldr mappend mempty

instance Vector Vector3 where
  vmap     f (V3 x y z)               = V3 (f x) (f y) (f z)
  vzipWith f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  vfold    f (V3 x y z)               = f z $ f x y

instance Vector Vector2 where
  vmap     f (V2 x y)            = V2 (f x) (f y)
  vzipWith f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
  vfold    f (V2 x y)            = f x y

instance Num Vector2 where
  (+)           = vzipWith (+)
  (*)           = undefined -- Vec -> Scalar
  abs           = undefined -- Vec -> Scalar
  negate        = vmap (*(-1))
  -- | Signum can be though of as the direction of a vector
  signum        = vmap signum
  fromInteger i = V2 (fromInteger i) 0

instance Num Vector3 where
  (+)           = vzipWith (+)
  (*)           = undefined -- Vec -> Scalar
  abs           = undefined -- Vec -> Scalar
  negate        = vmap (*(-1))
  -- | Signum can be though of as the direction of a vector
  signum        = vmap signum
  fromInteger i = V3 (fromInteger i) 0 0

instance Show Vector2 where
  show (V2 x y) = "(" ++ show x ++ " x, " ++ show y ++ " y)"

instance Show Vector3 where
  show (V3 x y z) = "(" ++ show x ++ " x, "
                        ++ show y ++ " y, "
                        ++ show z ++ " z)"

instance Ord Vector2 where
  compare v1 v2 = compare (magnitude v1) (magnitude v2)

instance Ord Vector3 where
  compare v1 v2 = compare (magnitude v1) (magnitude v2)

instance Eq Vector2 where
  (V2 x y) == (V2 x' y') = x == x' && y == y'

instance Eq Vector3 where
  (V3 x y z) == (V3 x' y' z') = x == x' && y == y' && z == z'
