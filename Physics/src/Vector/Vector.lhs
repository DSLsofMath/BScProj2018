\begin{code}
module Vector.Vector where

import Test.QuickCheck hiding (scale)
\end{code}

Vectors in two dimensions.
-----------------------------------------------------------

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
\begin{spec}
magnitude :: Vector2 -> Scalar
magnitude (V2 x y) = sqrt (x^2 + y^2)
\end{spec}

And now we can calulate the magnitude of a vector in two dimensions:
```
  Vector> let vec = V2 5 3
  Vector> magnitude vec
  7.0710678118654755
```

Addition and subtraction of vectors is acamplished using the components of the
vectors. For instance when adding the forces (vectors) acting on a body we would
add the components of the forces acting in the $x$ direction and the components
in the $y$ direction. So our functions for adding and subtracting vectors in two
dimensions are:

\begin{code}
add2 :: Vector2 -> Vector2 -> Vector2
add2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

sub2 :: Vector2 -> Vector2 -> Vector2
sub2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
\end{code}

But this only works for two vectors. In reality we might be working with several
hundreds of vectors so it would be useful to add, for instance a list of
vectors together and get one final vector as a result. We can use \textit{foldr}
using the zero vector as a starting value.
to acomplish this.
\begin{code}
addListOfVectors :: [Vector2] -> Vector2
addListOfVectors = foldr add zeroVector
  where
    zeroVector = V2 0 0
\end{code}

Let's try it out!

```
  *Vector> let vec1 = V2 5 3
  *Vector> let vec2 = V2 6 5
  *Vector> sub2 vec1 vec2

  <interactive>:23:1: error:
      • No instance for (Show Vector2) arising from a use of ‘print’
      • In a stmt of an interactive GHCi command: print it
```

The interpreter is complaining that it doesn't know how to interpret our
datatype for vectors as a string. The easy solution would be to just derive our
instance for Show, but to really solidify the fact that we are working with
coordinates let's make our own instance for Show.

\begin{code}
instance Show Vector2 where
  show (V2 x y) = "(" ++ show x ++ " x, " ++ show y ++ " y)"
\end{code}

And let's try our example again:
```
  *Vector> let vec1 = V2 5 3
  *Vector> let vec2 = V2 6 5
  *Vector> sub2 vec1 vec2
  (-1.0 x, -2.0 y)
```

And let's also try adding a list of vectors using our new function:

```
  *Vector> let vec3 = V2 8 9
  *Vector> let vectors = [vec1, vec2, vec3]
  *Vector> addListOfVectors vectors
  (19.0 x, 17.0 y)
```

It works!

We can also multiply a vector by a scalar. This is also done componentwise.
We'll call this scaling a vector. So we could make double a vector by
multiplying it with $2.0$ and halving it by multiplying it with $0.5$.

\begin{spec}
scale :: Scalar -> Vector2 -> Vector2
scale factor (V2 x y) = V2 (factor * x) (factor * y)
\end{spec}

Combining this with the unit vectors:

\begin{code}
unitX :: Vector2
unitX = V2 1 0

unitY :: Vector2
unitY = V2 0 1
\end{code}

We get a new way of making vectors, namely by scaling the unit vectors and
adding them together. Let's create the vector (5 x, 3 y) using this approach.

```shell
  *Vector> add (scale 5 unitX) (scale 3 unitY)
  (5.0 x, 3.0 y)
```

In order to check that this vector is actually equal to the vector created using
the contructor \textit{V2} we need to make our vector an instance of
\textit{Eq}.

\begin{spec}
instance Eq Vector2 where
  (V2 x1 y1) == (V2 x2 y2) = (x1 == x2) && (y1 == y2)
\end{spec}


Let's try it out:

```
  *Vector> let vec1 = V2 5 3
  *Vector> let vec2 =  add (scale 5 unitX) (scale 3 unitY)
  *Vector> vec1 == vec2
  True
```
![](http://i.imgur.com/GMCn5mi.png)

We have one final important operation left to define for vectors in two
dimensions, the dot product. The formula is quite simple:
\begin{equation}
  \vec{a} \cdot \vec{b} = a_x \cdot b_x + a_y \cdot b_y
\end{equation}

And our function simply becomes:

\begin{spec}
dotProd :: Vector2 -> Vector2 -> Scalar
dotProd (V2 ax ay) (V2 bx by) = ax * bx + ay * by
\end{spec}

But this doesn't give us any intuition about what it means to take the dot
product between vectors. The common interpretation is "geometric projection",
but that only makes sense if you already understand the dot product. Let's try
to give an easier analogy using the dash panels (boost pads) from \textit{Mario
Kart}. The dash panel is designed to give you boost of speed in a specific
direction, usually straight forward. So the vector associated with the dash
panel can be represented with a unit vector multiplied with some factor of
boost, say 10.

\begin{code}
dashPanel :: Vector2
dashPanel = scale 10 unitY
\end{code}

Now let's say that your cart has this arbitrarily chosen velocity vector:

\begin{code}
cart :: Vector2
cart = V2 3 5
\end{code}

Depending on which angle you hit the dash panel you'll receive different
amounts of boost. Since the $x$-component of the dashPanel is 0 any component
of speed on the $x$-direction will be reduced to zero. Only the speed in the
direction of $y$ will be boosted. But there are worse ways to hit the dash
panel. We could for instance create a new velocity vector with the exact same
magnitude of speed but which would recieve a worse boost.

\begin{code}
worseCart :: Vector2
worseCart = V2 5 3
\end{code}

Let's see this in action.
```
*Vector> magnitude cart == magnitude worseCart
True
*Vector> dotProd dashPanel cart
50.0
*Vector> dotProd dashPanel worseCart
30.0
```

We talked a lot about angles between vectors but we havn't used it in our code,
so lets make a function which calculates the angle of a vector. The formula is
as follows:
%TODO
\textbf{Insert picture of angle of triangle here}

We'll use Doubles to represent the angle.
\begin{code}
type Angle  = Double

angle :: Vector2 -> Scalar
angle (V2 x y) = atan y/x
\end{code}

Using angles and magnitudes we can even make a new function for making vectors:

\begin{code}
mkVector :: Scalar -> Angle -> Vector2
mkVector mag angle = V2 x y
  where
    x = mag * cos angle
    y = mag * sin angle
\end{code}

%-- Angle between two vectors
%angleG :: Vector vec => vec -> vec -> Scalar
%angleG v1 v2 = (dotProd v1 v2) / ((magnitude v1) * (magnitude v2))

Vectors in three dimensions.
--------------------------------------

The datatype for a vector in three dimensions is basically the same as vector
in two dimensions, we'll just add a $z$-component.

\begin{code}
data Vector3 = V3 Scalar Scalar Scalar
\end{code}

Similarily the functions for adding three dimensional vectors:
\begin{code}
add3 :: Vector3 -> Vector3 -> Vector3
add3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
\end{code}

Multiplying with a scalar:
\begin{code}
scale3 :: Scalar -> Vector3 -> Vector3
scale3 fac (V3 x y z) = V3 (fac * x) (fac * y) (fac * z)
\end{code}

And for calculating the magnitude:
\begin{code}
mag3 :: Vector3 -> Scalar
mag3 (V3 x y z) = sqrt $ x**2 + y**2 + z**2
\end{code}

Looks earily similar to our functions for vectors in two dimensions. This
suggest that there might be a better way to handle this, in order to avoid
repeating ourselves.

Addition and subtraction on vectors works by "unpacking" the vectors, taking
their components, applying some function to them (+/-) and then packing them up
as a new vector. This is very similar to the Haskell function \textit{zipWith}
which works over lists instead of vectors.

\begin{spec}
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
\end{spec}

When we're multiplying with a scalar we again unpack the vector and then apply
mulitiplication with a factor to each component before packing it up again.
This is quite similar to the Haskell function \textit{map}, which again works
over lists.

\begin{spec}
map :: (a -> b) -> [a] -> [b]
\end{spec}

When calculating the magnitude of a vector we first unpack the vector and then
apply $^2$ to each component of the vector. This is doable with aformentioned
\textit{map}. We then \textit{fold} the components together using $+$ which
results in a final scalar value. Those of you familliar with functional
languages will know where I'm going with this, those of you who aren't will
hopefully understand where I'm going when reading the examples.

Using this information we will now create a new class for vectors which implement this functionality:

\begin{code}
class Vector vector where
  vmap      :: (Scalar -> Scalar) -> vector  -> vector
  vzipWith  :: (Scalar -> Scalar  -> Scalar) -> vector -> vector -> vector
  vfold     :: (Scalar -> Scalar  -> Scalar) -> vector -> Scalar
\end{code}

Now we have a blueprint for what vector is, so let's implement it for our own vector datatypes.
\begin{code}
instance Vector Vector2 where
  vmap     f (V2 x y)            = V2 (f x)    (f y)
  vzipWith f (V2 x y) (V2 x' y') = V2 (f x x') (f y y')
  vfold    f (V2 x y)            = f x y

instance Vector Vector3 where
  vmap     f (V3 x y z)               = V3 (f x)    (f y)    (f z)
  vzipWith f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')
  vfold    f (V3 x y z)               = f z $ f x y
\end{code}

Now we're finally leveraging the power of the Haskell typesystem!

We can now implement more generalized functions for addition and subtraction
between vectors.

\begin{code}
add :: Vector vec => vec -> vec -> vec
add = vzipWith (+)

sub :: Vector vec => vec -> vec -> vec
sub = vzipWith (-)
\end{code}

For multiplying with a scalar:
\begin{code}
scale :: Vector vec => Scalar -> vec -> vec
scale factor = vmap (* factor)
\end{code}

And for calculating the magnitude of a vector:
\begin{code}
magnitude :: Vector vec => vec -> Scalar
magnitude v = sqrt . vfold (+) $ vmap (**2) v
\end{code}

We can even use it to make a generalized function for calculating the dot product.
\begin{code}
dotProd :: Vector vec => vec -> vec -> Scalar
dotProd v1 v2 = vfold (+) $ vzipWith (*) v1 v2
\end{code}

Text needed herererere
-----------
\begin{code}
angleBetween ::Vector vec => vec -> vec -> Scalar
angleBetween a b = acos $ numerator / denominator
  where
    numerator = vfold (+) $ vzipWith (*) a b
    denominator = magnitude a * magnitude b

dotProd' :: Vector vec => vec -> vec -> Scalar
dotProd' a b = magnitude a * magnitude b * cos (angleBetween a b)
\end{code}
Cross Product
---------------------------------

We have one final function left to define, the cross product. The formula is as follows:
\begin{equation}
  \vec{a} \times \vec{b} = |\vec{a}| \cdot |\vec{b}| \cdot sin(\theta)
\end{equation}

Where $\theta$ is the angle between the vectors. And $|\vec{a}|$, $|\vec{b}|$
are the magnitudes of the vectors.

So our function for calculating the cross product becomes:

\begin{spec}
crossProd' :: Vector3 -> Vector3 -> Vector3
crossProd' a b = (magnitude a) * (magnitude b) * sin (angleBetween a b)
  where
    angleBetween :: (Vector vec) => vec -> vec -> Scalar
    angleBetween v1 v2 = acos ((dotProd v1 v2) / ((magnitude v1) * (magnitude v2)))
\end{spec}

\begin{code}
crossProd :: Vector3 -> Vector3 -> Vector3
crossProd (V3 x y z) (V3 x' y' z') = V3 (y*z' - z*y') -- X
                                        (z*x' - x*z') -- Y
                                        (x*y' - y*x') -- Z
\end{code}

% lift :: Vector2 -> Vector3
% lift (V2 x y) = V3 x y 0

% Lrigth
% prop_crossProd :: Vector3 -> Bool
% prop_crossProd v = (crossProd v v) == 0
%
% -- | TODO
% -- | ~Laws~
% -- | Langrange's formula: a x (b x c) = b(a * c) - c(a * b)
% -- | Cross product is anticommutative
% -- | Jacobi identity: a x (b x c) + b x (c x a) + c x (a x b) = 0

Quickcheck!
--------------------------------

There are certain laws or preperties that vectors adher to, for example the
jacobi identity:
\begin{equation}
 \vec{a} \times (\vec{b} \times \vec{c}) + \vec{b} \times (\vec{c} \times
 \vec{a}) + \vec{c} \times (\vec{a} \times \vec{b}) = 0
\end{equation}
Or that the cross product is anticommutative. We can't actually prove these in a
meaningful way without a whole bunch of packages and pragmas, but we can
quickcheck them. But to do that we need to be able to generate vectors, so must
make out vectors an instance of \textit{Arbitrary}.

We do this by generating arbitrary scalars and then constructing vectors with
them.
\begin{code}
instance Arbitrary Vector2 where
  arbitrary = arbitrary >>= (\(s1, s2) -> return $ V2 s1 s2)

instance Arbitrary Vector3 where
  arbitrary = arbitrary >>= (\(s1, s2, s3) -> return $ V3 s1 s2 s3)
\end{code}

Let's try it out!
```
ghci> generate arbitrary :: IO Vector2
(-26.349975377051404 x, 9.71134047527185 y)
```

Seems pretty random to me.

Now we can check some properties, lets' start with commutativity of vector
addition:
\begin{equation}
  \vec{a} + \vec{b} = \vec{b} + \vec{a}
\end{equation}

Which translates to:
\begin{code}
prop_CommutativityAddition :: Vector3 -> Vector3 -> Bool
prop_CommutativityAddition v1 v2 = v1 + v2 == v2 + v1
\end{code}

And we test this in the \textit{repl}.
```
ghci> quickCheck prop_CommutativityAddition
+++ OK, passed 100 tests.
```

And associativity of addition:
\begin{equation}
  \vec{a} + (\vec{b} + \vec{c}) = (\vec{a} + \vec{b}) + \vec{c}
\end{equation}

\begin{code}
prop_AssociativityAddition :: Vector2 -> Vector2 -> Vector2 -> Bool
prop_AssociativityAddition a b c = a + (b + c) == (a + b) + c
\end{code}

```
ghci> quickCheck prop_AssociativityAddition
*** Failed! Falsifiable (after 2 tests):
(0.5240133611343812 x, -0.836882545823441 y, -4776.775557184785 z)
(-0.17261751005585407 x, 0.7893754200476363 y, -0.19757165887775568 z)
(0.3492200657348603 x, 0.10861834028920295 y, 0.45838513657221946 z)
```

This is strange because the laws should always be correct. But this error stems
from the fact that we're using a computer and that using doubles (Scalar) will
introduce approximation errors. We can fix this by relaxing our instance for
\textit{Eq} and only requiring the comonents of the vectors to be approximately
equal.

\begin{code}
eps :: Double
eps = 1 * (10 ** (-6))

instance Eq Vector2 where
  (V2 x1 y1) == (V2 x2 y2) = xCheck && yCheck
    where
      xCheck = abs (x1 - x2) <= eps
      yCheck = abs (y1 - y2) <= eps
\end{code}

Let's try again.
```
*Vector.Vector> quickCheck prop_AssociativityAddition
+++ OK, passed 100 tests.
```

More laws
-------------------

Dot product is commutative:

\begin{code}
prop_dotProdCommutative :: Vector3 -> Vector3 -> Bool
prop_dotProdCommutative a b = dotProd a b == dotProd b a
\end{code}

In order to check some laws which depends on checking the equality of scalars
we'll introduce a function which checks that two scalars are approximately
equal.
\begin{code}
-- Approx equal
(~=) :: Scalar -> Scalar -> Bool
rhs ~= lhs = abs (rhs - lhs) <= eps
\end{code}

Dot product is distributive over addition:
![Dot product distributive, (C)](https://upload.wikimedia.org/wikipedia/commons/a/aa/Dot_product_distributive_law.svg)

\begin{equation}
\vec{a} \cdot (\vec{b} + \vec{c}) = (\vec{a} \cdot \vec{b}) + (\vec{a} \cdot
\vec{c})
\end{equation}

\begin{code}
prop_dotProdDistrubitiveAddition ::  Vector2 -> Vector2 -> Vector2 -> Bool
prop_dotProdDistrubitiveAddition a b c = dotProd a (b + c) ~= (dotProd a b + dotProd a c)
\end{code}

The dot product is homogeneous under scaling in each variable:
\begin{equation}
(x * \vec{a}) \cdot b = x * (\vec{a} \cdot \vec{b}) = \vec{a} \cdot (x * \vec{b})
\end{equation}

\begin{code}
prop_dotProdHomogeneousScaling :: Scalar -> Vector3 -> Vector3 -> Bool
prop_dotProdHomogeneousScaling x a b = e1 == e2 && e2 == e3
  where
    e1 = dotProd (scale 0 a) b
    e2 = 0 * dotProd a b
    e3 = dotProd a (scale 0 b)
\end{code}

The cross product of a vector with itself is the zero vector.
\begin{code}
prop_crossProd_with_self :: Vector3 -> Bool
prop_crossProd_with_self v = crossProd v v == 0
\end{code}

The crossproduct is anticommutative:
\begin{equation}
\vec{a} \times \vec{b} = - (\vec{b} \times \vec{a})
\end{equation}

\begin{code}
prop_crossProdAntiCommutative :: Vector3 -> Vector3 -> Bool
prop_crossProdAntiCommutative v1 v2 = v1 * v2 == - (v2 * v1)
\end{code}

The cross product is distributive over addition:
\begin{equation}
\vec{a} \times (\vec{b} + \vec{c}) = (\vec{a} \times \vec{b}) + (\vec{a} \times
\vec{c})
\end{equation}

\begin{code}
prop_crossProdDistrubitiveAddition :: Vector3 -> Vector3 -> Vector3 -> Bool
prop_crossProdDistrubitiveAddition a b c = a * (b + c) == (a * b) + (a * c)
\end{code}

Vector triple product (Lagrange's formula).

\begin{equation}
\vec{a} \times (\vec{b} \times \vec{c}) = \vec{b}(\vec{a} \cdot \vec{c}) -
                                \vec{c}(\vec{a} \cdot \vec{b})
\end{equation}

\begin{code}
prop_lagrange :: Vector3 -> Vector3 -> Vector3 -> Bool
prop_lagrange a b c = a * (b * c) == (scale (dotProd a c) b -
                                      scale (dotProd a b) c)
\end{code}

The Jacobi identity:
\begin{equation}
\vec{a} \times (\vec{b} \times \vec{c}) +
\vec{b} \times (\vec{c} \times \vec{a}) +
\vec{v} \times (\vec{a} \times \vec{b})
\end{equation}

\begin{code}
prop_JacobiIdentity :: Vector3 -> Vector3 -> Vector3 -> Bool
prop_JacobiIdentity a b c = a * (b * c) +
                            b * (c * a) +
                            c * (a * b) == 0
\end{code}

Fun instances
------------------------------------
\begin{code}
instance Monoid Vector2 where
  mempty  = zeroVector
  mappend = (+)
  mconcat = foldr mappend mempty

instance Monoid Vector3 where
  mempty  = zeroVector
  mappend = (+)
  mconcat = foldr mappend mempty


instance Num Vector2 where
  (+)           = vzipWith (+)
  (*)           = undefined -- Crossproduct not defined for Vector2
  abs           = vmap abs
  negate        = vmap (*(-1))
  -- | Signum can be though of as the direction of a vector
  signum        = vmap signum
  fromInteger i = V2 (fromInteger i) 0

instance Num Vector3 where
  (+)           = vzipWith (+)
  (*)           = crossProd
  abs           = vmap abs
  negate        = vmap (*(-1))
  -- | Signum can be though of as the direction of a vector
  signum        = vmap signum
  fromInteger i = V3 (fromInteger i) 0 0

-- TODO: Explain why this works
zeroVector :: (Vector a, Num a) => a
zeroVector = 0

instance Show Vector3 where
  show (V3 x y z) = "(" ++ show x ++ " x, "
                        ++ show y ++ " y, "
                        ++ show z ++ " z)"
instance Ord Vector2 where
  compare v1 v2 = compare (magnitude v1) (magnitude v2)

instance Ord Vector3 where
  compare v1 v2 = compare (magnitude v1) (magnitude v2)

instance Eq Vector3 where
  (V3 x y z) == (V3 x' y' z') = xCheck && yCheck && zCheck
    where
    xCheck = abs (x - x') <= eps
    yCheck = abs (y - y') <= eps
    zCheck = abs (z - z') <= eps
\end{code}

\begin{code}
runTests :: IO ()
runTests = do
  putStrLn "Commutativity of vector addition:"
  quickCheck prop_CommutativityAddition
  putStrLn "Associativity of vector addition:"
  quickCheck prop_AssociativityAddition
  putStrLn "Dot product distributive over addition:"
  quickCheck prop_dotProdDistrubitiveAddition
  putStrLn "Homogeneous scaling:"
  quickCheck prop_dotProdHomogeneousScaling
  putStrLn "Commutative dot product:"
  quickCheck prop_dotProdCommutative
  putStrLn "Crossproduct of a vector with itself:"
  quickCheck prop_crossProd_with_self
  putStrLn "Cross product is anticommutative"
  quickCheck prop_crossProdAntiCommutative
  putStrLn "Cross product distributive over addition"
  quickCheck prop_crossProdDistrubitiveAddition
  putStrLn "Lagrange formula"
  quickCheck prop_lagrange
  putStrLn "Jacobi identity"
  quickCheck prop_JacobiIdentity
\end{code}
