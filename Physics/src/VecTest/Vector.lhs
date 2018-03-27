> module VecTest.Vector where

> import Dimensions.Quantity
> import Dimensions.TypeLevel as T
> import Calculus.Calculus
> import Prelude hiding (length)

Vectors in two dimensions.
-----------------------------------------------------------

> type Scalar = Expr

> data Vector2 = V2 Scalar Scalar
> type Angle  = Double

> data TestVector dim = V (Quantity dim Scalar) (Quantity dim Scalar)
>   deriving Show

> tv1 = V (3 # length) (5 # length)
> tv2 = V (6 # length) (1 # length)


> instance Vector (TestVector dim) where
>   vmap f (V x y)                 = V (qmap' f x) (qmap' f y)
>   vzipWith f (V x1 y1) (V x2 y2) = V (qfold f x1 x2) (qfold f y1 y2)
>   vfold f (V x1 y1)              = lift $ qfold f x1 y1

> angle :: Vector2 -> Scalar
> angle (V2 x y) = atan y/x

Using angles and magnitudes we can even make a new function for making vectors:

> mkVector mag angle = V2 x y
>   where
>     x = mag * cos angle
>     y = mag * sin angle

> data Vector3 = V3 Scalar Scalar Scalar

> class Vector vector where
>   vmap      :: (Scalar -> Scalar) -> vector  -> vector
>   vzipWith  :: (Scalar -> Scalar  -> Scalar) -> vector -> vector -> vector
>   vfold     :: (Scalar -> Scalar  -> Scalar) -> vector -> Scalar

> instance Vector Vector2 where
>   vmap     f (V2 x y)            = V2 (f x)    (f y)
>   vzipWith f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
>   vfold    f (V2 x y)            = f x y

> instance Vector Vector3 where
>   vmap     f (V3 x y z)               = V3 (f x)    (f y)    (f z)
>   vzipWith f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
>   vfold    f (V3 x y z)               = f z $ f x y

Now we're finally leveraging the power of the Haskell typesystem!

We can now implement more generalized functions for addition and subtraction
between vectors.

> add :: Vector vec => vec -> vec -> vec
> add v1 v2 = vzipWith (+) v1 v2
 
> sub :: Vector vec => vec -> vec -> vec
> sub v1 v2 = vzipWith (-) v1 v2

> scale :: Vector vec => Scalar -> vec -> vec
> scale factor vector = vmap (* factor) vector

> magnitude :: Vector vec => vec -> Scalar
> magnitude v = sqrt . vfold (+) $ vmap (**2) v

> dotProd :: Vector vec => vec -> vec -> Scalar
> dotProd v1 v2 = vfold (*) $ vzipWith (+) v1 v2

Cross Product
---------------------------------

We have one final function left to define, the cross product. The formula is as follows:
\begin{equation}
  \vec{a} \times \vec{b} = |\vec{a}| \cdot |\vec{b}| \cdot sin(\theta)
\end{equation}

Where $\theta$ is the angle between the vectors. And $|\vec{a}|$, $|\vec{b}|$
are the magnitudes of the vectors.

So our function for calculating the cross product becomes:

%\begin{code}
%crossProd :: Vector3 -> Vector3 -> Vector3
%crossProd a b = (magnitude a) * (magnitude b)
%\end{code}

\begin{spec}
crossProd :: Vector3 -> Vector3 -> Vector3
crossProd (V3 x y z) (V3 x' y' z') = V3 (y*z' - z*y') -- X
                                        (z*x' - x*z') -- Y
                                        (x*y' - y*x') -- Z
\end{spec}

% lift :: Vector2 -> Vector3
% lift (V2 x y) = V3 x y 0
 
% 
% prop_crossProd :: Vector3 -> Bool
% prop_crossProd v = (crossProd v v) == 0
% 
% -- | TODO
% -- | ~Laws~
% -- | Langrange's formula: a x (b x c) = b(a * c) - c(a * b)
% -- | Cross product is anticommutative
% -- | Jacobi identity: a x (b x c) + b x (c x a) + c x (a x b) = 0

Fun instances
------------------------------------

> instance Monoid Vector2 where
>   mempty  = zeroVector
>   mappend = (+)
>   mconcat = foldr mappend mempty
> 
> instance Monoid Vector3 where
>   mempty  = zeroVector
>   mappend = (+)
>   mconcat = foldr mappend mempty
> 
> 
> instance Num Vector2 where
>   (+)           = vzipWith (+)
>   (*)           = undefined -- Vec -> Scalar
>   abs           = undefined -- Vec -> Scalar
>   negate        = vmap (*(-1))
>   -- | Signum can be though of as the direction of a vector
>   signum        = vmap signum
>   fromInteger i = V2 (fromInteger i) 0
> 
> instance Num Vector3 where
>   (+)           = vzipWith (+)
>   (*)           = undefined -- Vec -> Scalar
>   abs           = undefined -- Vec -> Scalar
>   negate        = vmap (*(-1))
>   -- | Signum can be though of as the direction of a vector
>   signum        = vmap signum
>   fromInteger i = V3 (fromInteger i) 0 0
> 
> zeroVector :: (Vector a, Num a) => a
> zeroVector = 0

> instance Show Vector2 where
>   show (V2 x y) = "(" ++ show x ++ " x, "
>                       ++ show y ++ " y)"
 
> instance Show Vector3 where
>   show (V3 x y z) = "(" ++ show x ++ " x, "
>                         ++ show y ++ " y, "
>                         ++ show z ++ " z)"

< instance Ord Vector2 where
<   compare v1 v2 = compare (magnitude v1) (magnitude v2)
 
< instance Ord Vector3 where
<   compare v1 v2 = compare (magnitude v1) (magnitude v2)
 
> instance Eq Vector3 where
>   (V3 x y z) == (V3 x' y' z') = x == x' && y == y' && z == z'


