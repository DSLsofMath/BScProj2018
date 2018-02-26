> module Vector where


A physical quantity that has only a magnitude is called a scalar.
In Haskell we'll represent this using a Double.
\begin{code}
  type Scalar = Double
\end{code}

A vector is a quantity that has both a magnitude and a direction. For instance
the \textit{velocity} of a moving body involves its speed (magnitude) and the
direction of motion. 

We cen represent the direction of a vector in two dimensions using its $x$ and
$y$ coordinates, which are both scalars. The direction is then given by the
angle between these coordinates and the origin (0,0).
\begin{code}
  data Vector2 = V2 Scalar Scalar
\end{code}
 
The magnitude of the vector is it's length. We can calculate this using
Pythagorean theorem:
\begin{equation}
  x^2 + y^2 = mag^2 
\end{equation}

In haskell this would be:
\begin{code}
  magnitude :: Vector2 -> Scalar
  magnitude (V2 x y) = sqrt $ x^2 + y^2
\end{code}

And now we can calulate the magnitude of a vector in two dimensions:
\begin{example}
  Vector> let vec = V2 5 3
  Vector> magnitude vec
  7.0710678118654755
\end{example}

Addition and subtraction of vectors is acamplished using the components of the
vectors. For instance when adding the forces (vectors) acting on a body we would
add the components of the forces acting in the $x$ direction and the components
in the $y$ direction. So our functions for adding and subtracting vectors in two
dimensions are:

\begin{code}
  add :: Vector2 -> Vector2 -> Vector2
  add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
  
  sub :: Vector2 -> Vector2 -> Vector2
  sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
\end{code}

But this only works for two vectors. In reality we might be working with several
hundreds of vectors so it would be useful to add, for instance a list of
vectors together and get one final vector as a result. We can use \textit{foldr}
using the zero vector as a starting value.
to acomplish this.
\begin{code}
  addListOfVectors :: [Vector2] -> Vector2
  addListOfVectors vectors = foldr add zeroVector vectors
    where
      zeroVector = V2 0 0
\end{code}

Let's try it out!

\begin{example}
  *Vector> let vec1 = V2 5 3
  *Vector> let vec2 = V2 6 5
  *Vector> sub vec1 vec2

  <interactive>:23:1: error:
      • No instance for (Show Vector2) arising from a use of ‘print’
      • In a stmt of an interactive GHCi command: print it
\end{example}

The interpreter is complaining that it doesn't know how to interpret our
datatype for vectors as a string. The easy solution would be to just derive our
instance for Show, but to really solidify the fact that we are working with
coordinates let's make our own instance for Show.

\begin{code}
  instance Show Vector2 where
    show (V2 x y) = "(" ++ show x ++ " x, " ++ show y ++ " y)"
\end{code}

And let's try our example again:
\begin{example}
  *Vector> let vec1 = V2 5 3
  *Vector> let vec2 = V2 6 5
  *Vector> sub vec1 vec2
  (-1.0 x, -2.0 y)
\end{example}

And let's also try adding a list of vectors using our new function:

\begin{example}
  *Vector> let vec3 = V2 8 9
  *Vector> let vectors = [vec1, vec2, vec3]
  *Vector> addListOfVectors vectors
  (19.0 x, 17.0 y)
\end{example}

It works!

We can also multiply a vector by a scalar. This is also done componentwise.
We'll call this scaling a vector. So we could make double a vector by
multiplying it with $2.0$ and halving it by multiplying it with $0.5$.

\begin{code}
  scale :: Scalar -> Vector2 -> Vector2
  scale factor (V2 x y) = V2 (factor * x) (factor * y)
\end{code}

Combining this with the unit vectors:

\begin{code}
  unitX :: Vector2
  unitX = V2 1 0
  
  unitY :: Vector2
  unitY = V2 0 1
\end{code}

We get a new way of making vectors, namely by scaling the unit vectors and
adding them together. Let's create the vector (5 x, 3 y) using this approach.

\being{example}
  *Vector> add (scale 5 unitX) (scale 3 unitY)
  (5.0 x, 3.0 y) 
\end{example}

In order to check that this vector is actually equal to the vector created using
the contructor \textit{V2} we need to make our vector an instance of
\textit{Eq}. 

\begin{code}
  instance Eq Vector2 where
    (V2 x1 y1) == (V2 x2 y2) = (x1 == x2) && (y1 == y2)
\end{code}

Let's try it out:

\begin{example}
  *Vector> let vec1 = V2 5 3
  *Vector> let vec2 =  add (scale 5 unitX) (scale 3 unitY)
  *Vector> vec1 == vec2
  True
\end{example}

data Vector3 = V3 Scalar Scalar Scalar

class Vector vec where
  vmap      :: (Scalar -> Scalar) -> vec -> vec
  vzipWith  :: (Scalar -> Scalar  -> Scalar) -> vec -> vec -> vec
  vfold     :: (Scalar -> Scalar  -> Scalar) -> vec -> Scalar

dotProd :: Vector vec => vec -> vec -> Scalar
dotProd v1 v2 = vfold (*) $ vzipWith (+) v1 v2

magnitude :: Vector vec => vec -> Scalar
magnitude v = sqrt . vfold (+) $ vmap (**2) v


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

type Angle  = Double

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
