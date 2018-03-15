> module Vector.Vector where

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
addListOfVectors vectors = foldr add zeroVector vectors
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

\begin{code}
instance Eq Vector2 where
  (V2 x1 y1) == (V2 x2 y2) = (x1 == x2) && (y1 == y2)
\end{code}

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
add v1 v2 = vzipWith (+) v1 v2

sub :: Vector vec => vec -> vec -> vec
sub v1 v2 = vzipWith (-) v1 v2
\end{code}

For multiplying with a scalar:
\begin{code}
scale :: Vector vec => Scalar -> vec -> vec
scale factor vector = vmap (* factor) vector
\end{code}

And for calculating the magnitude of a vector:
\begin{code}
magnitude :: Vector vec => vec -> Scalar
magnitude v = sqrt . vfold (+) $ vmap (**2) v
\end{code}

We can even use it to make a generalized function for calculating the dot product.
\begin{code}
dotProd :: Vector vec => vec -> vec -> Scalar
dotProd v1 v2 = vfold (*) $ vzipWith (+) v1 v2
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
crossProd :: Vector3 -> Vector3 -> Vector3
crossProd a b = (magnitude a) * (magnitude b)
\end{spec}

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
  (V3 x y z) == (V3 x' y' z') = x == x' && y == y' && z == z'
\end{code}
