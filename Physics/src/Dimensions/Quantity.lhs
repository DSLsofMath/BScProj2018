
Quantities
==========

We will now create a data type for quantities and combine units on value-level and type-level. Just as before, a bunch of GHC-extensions are necessary.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE TypeOperators #-}

> module Units.Quantity
> ( length
> , mass
> , time
> , current
> , temperature
> , substance
> , luminosity
> , one
> , velocity
> , acceleration
> , force
> , momentum
> , (#)
> , (+#)
> , (-#)
> , (*#)
> , (/#)
> , sinq
> , cosq
> , asinq
> , acosq
> , atanq
> , expq
> , logq
> )
> where

> import qualified Units.ValueLevel as V
> import           Units.TypeLevel  as T
> import           Prelude          as P hiding (length, div)

The data type
-------------

First we create the data type for quantities.

[PaJa: förklara gärna varför typ-arg. inte är i samma ordning som värde-arg.]

> data Quantity (u :: T.Unit) (v :: *) where
>   Quantity :: v -> V.Unit -> Quantity u v

- `data Quantity` creates a *type*.
- `u` is a *type* and `T.Unit` is a *kind*. This makes our data type have a type parameter where the type must have the kind `T.Unit`.
- `v :: *` means that `v`, as in **v**alue, has the kind `*`, which is the kind of types that can have values. `v` could for instance be `Double` or `Integer`.
- `Quantity` on the row below is a value constructor.
- The value constructor has two *value*-parameters which should have certain *types*.
  - `v` is a type which represents a number.
  - `V.Unit` is the unit of the quantity on value-level.
- `Quantity` on the upper row is the name of a type (or rather a type constructor since it has the two type-parameters `u` and `v`) while `Quantity` on the lower row is the name of a value (value constructor). Same names but different things. It's possible to do this, just like the definition below is possible, with the same name on different things on different sides of the equals-sign.

The corresponding data type without units on type-level would look like

[PaJa: jag föreslår att ni använder GADT-syntax även här för att underlätta jämförelsen.]

< data Quantity v = Quantity V.Unit v

`u` in the prior definition should not be confused with `V.Unit` in the latter. `u` is the name of an unbound type, which should be of kind `T.Unit`, while `V.Unit` means that a value of type `V.Unit` should be here. Due to syntactical reasons it stands `u` instead of `T.Unit`, but one can think it stands `T.Unit` if it makes it easier to understand.

A taste of types
----------------

We will implement all arithmetic operations on `Quantity`, but for now, to get a taste of types, we show here addition and multiplication and some examples of values of type `Quantity`.

> quantityAdd :: (Num v) => Quantity u v ->
>                           Quantity u v ->
>                           Quantity u v
> quantityAdd (Quantity v1 u) (Quantity v2 _) = Quantity (v1+v2) u

The type is interpreted as follows: two values of type `Quantity u v` is the input, where `u` is the type-level unit. The output is also a value of `Quantity u v`.

The type of the function forces the inputs to have the same units. For this reason, the unit on value-level doesn't matter on one of the arguments, because they will be the same. (It's possible to create values where the units on value-level and type-level don't match. We'll come back to this later.)

Multiplication is

> quantityMul :: (Num v) => Quantity u1 v -> 
>                           Quantity u2 v ->             
>                           Quantity (u1 `Mul` u2) v
> quantityMul (Quantity v1 u1) (Quantity v2 u2) = 
>   Quantity (v1*v2) (u1 `V.mul` u2)

The type has the following interpretation: two valus of type `Quantity ux v` is input, where `ux` are two types representing (potentially different) units. As output, a value of type `Quantity` is returned. The type in the `Quantity` will be the type that is the product of the two units in.

Now on to some example values.

> width :: Quantity T.Length Double
> width = Quantity 0.5 V.length

> height :: Quantity T.Length Double
> height = Quantity 0.3 V.length

> type Area = Mul T.Length T.Length

The following example shows that during a multiplication, the types will change, as they should. The units change not only on value-level but also on type-level.

> area :: Quantity Area Double
> area = quantityMul width height

Having type-level units is used below, to enforce at compile-time that only allowed operations are performed.

< -- Doesn't even compile
< weird = quantityAdd width area

If the units only were value-level, this error would be noticed only at run-time.

Pretty-printer
--------------

Let's do a pretty-printer for quantities. Most of the work is already done by the units at value-level.

> showQuantity :: (Show v) => Quantity u v -> String
> showQuantity (Quantity v u) = show v ++ " " ++ show u

> instance (Show v) => Show (Quantity u v) where
>   show = showQuantity

Comparsions
-----------

It's useful to be able to compare quantities. Perhaps one wants to know which of two amounts of energy is the largest. But what's the largest of `1 J` and `1 m`? That's no meaningful comparsion since the units don't match. This behaviour is prevented by having type-level units.

> quantityEq :: (Eq v) => Quantity u v -> Quantity u v -> Bool
> quantityEq (Quantity v1 _) (Quantity v2 _) = v1 == v2
> 
> instance (Eq v) => Eq (Quantity u v) where
>   (==) = quantityEq

> 
> quantityCompare :: (Ord v) => Quantity u v -> 
>                               Quantity u v -> Ordering
> quantityCompare (Quantity v1 _) (Quantity v2 _) =
>   compare v1 v2

> instance (Ord v) => Ord (Quantity u v) where
>   compare = quantityCompare

Arithmetic on quantities
------------------------

Let's implement the arithmetic operations on `Quantity`. Basically it's all about creating functions with the correct type-level units.

> infixl 6 +#
> (+#) :: (Num v) => Quantity u v -> Quantity u v -> 
>                    Quantity u v
> (+#) = quantityAdd
> 
> infixl 6 -#
> (-#) :: (Num v) => Quantity u v -> Quantity u v -> 
>                    Quantity u v
> (Quantity v1 u) -# (Quantity v2 _) = Quantity (v1-v2) u
> 
> infixl 7 *#
> (*#) :: (Num v) => Quantity u1 v -> Quantity u2 v ->
>                    Quantity (u1 `Mul` u2) v
> (*#) = quantityMul
> 
> infixl 7 /#
> (/#) :: (Fractional  v) => Quantity u1 v -> 
>                            Quantity u2 v ->
>                            Quantity (u1 `Div` u2) v
> (Quantity v1 u1) /# (Quantity v2 u2) =
>   Quantity (v1 / v2) (u1 `V.div` u2)

For all operations on quantities, one does the operation on the value and, in the case of multiplication and division, on the units separetly. For addition and subtraction the in-units must be the same. Nothing then happens with the unit.

How does one perform operations such as `sin` on a quantity with a *potential* unit? The answer is that the quantity must be unitless, and with the unit nothing happens. For the follwing functions, the quantites must be unitless.

< sinq :: (Floating v) => Quantity One v -> Quantity One v
< sinq (Quantity v ul) = Quantity (sin v) ul
< 
< cosq :: (Floating v) => Quantity One v -> Quantity One v
< cosq (Quantity v ul) = Quantity (cos v) ul

We quickly realize a pattern, so let's generalize a bit.

> qmap :: (a -> b) -> Quantity One a -> Quantity One b
> qmap f (Quantity v ul) = Quantity (f v) ul
> 
> type UnaryScalar v = Quantity One v -> Quantity One v
> 
> sinq, cosq, asinq, acosq, atanq, expq, logq :: (Floating v) =>
>   UnaryScalar v
> sinq  = qmap sin
> cosq  = qmap cos
> asinq = qmap asin
> acosq = qmap acos
> atanq = qmap atan
> expq  = qmap exp
> logq  = qmap log

Why not make `Quantity` an instance of `Num`, `Fractional`, `Floating` och `Functor`? The reason is that the functions of those type classes have the following type

< (*) :: (Num a) => a -> a -> a

which isn't compatible with `Quantity` since multiplication with `Quantity` has the following type

< (*#) :: (Num v) => Quantity u1 v -> Quantity u2 v ->
<                    Quantity (u1 `Mul` u2) v

The input here may actually be of *different* types, and the output has a type depending on the types of the input. However, the *kind* of the inputs and output are the same, namely `Quantity`. We'll just have to live with not being able to make `Quantity` a `Num`-instance.

However, operations with only scalars (type `One`) has types compatible with `Num`. Therefore a possibility would've been to make `Quantity One` a `Num`-instance.

Syntactic sugar
---------------

In order to create a value representing a certain distance (5 meters, for example) one does the following

< distance :: Quantity T.Length Double
< distance = Quantity 5 V.length

Writing that way each time is very clumsy. You can also do "dumb" things such as

< distance :: Quantity T.Length Double
< distance = Quantity 5 V.time

with different units on value-level and type-level.

To solve these two problems we will introduce some syntactic sugar. First some pre-made values for the 7 base units and the scalar.

> length :: (Num v) => Quantity Length v
> length = Quantity 0 V.length
> 
> mass :: (Num v) => Quantity Mass v
> mass = Quantity 0 V.mass
> 
> time :: (Num v) => Quantity Time v
> time = Quantity 0 V.time
> 
> current :: (Num v) => Quantity Current v
> current = Quantity 0 V.current
> 
> temperature :: (Num v) => Quantity Temperature v
> temperature = Quantity 0 V.temperature
> 
> substance :: (Num v) => Quantity Substance v
> substance = Quantity 0 V.substance
> 
> luminosity :: (Num v) => Quantity Luminosity v
> luminosity = Quantity 0 V.luminosity
> 
> one :: (Num v) => Quantity One v
> one = Quantity 0 V.one

And now the sugar.

[PaJa: här skulle man kunna använda multiplikation, ifall enheterna hade 1 som värde. Kommentera eller implementera.]

> (#) :: (Num v) => v -> Quantity u v -> Quantity u v
> v # (Quantity _ u) = Quantity v u

The intended usage of the function is the following

< ghci> let myDistance = 5 # length
< ghci> :t myDistance
< t :: Num v => Quantity Length v
< ghci> myDistance
< 5 m

To create a `Quantity` with a certain value (here `5`) and a certain unit (here `length`), you write as above and get the correct unit on both value-level and type-level.

`length`, `mass` and so on are just dummy-values with the correct unit (on both value-level and type-level) in order to easier create `Quantity` values. Any value with the correct unit on both levels can be used as the right hand argument.

< ghci> let otherPersonsDistance = 10 # length
< ghci> let myDistance = 5 # otherPersonsDistance
< ghci> :t myDistance
< t :: Num v => Quantity Length v
< ghci> myDistance
< 5 m

Precisely the same result.

We want to maintain the invariant that the unit on value-level and type-level always match. The pre-made values from above maintain it, and so does the arithmetic operations we previously created. Therefore, we only export those from this module! The user will have no choice but to use these constructs and hence the invariant will be maintained.

If the user needs other units than the base units (which it probably will), the follwing example shows how it's done.

< ghci> let myLength = 5 # length
< ghci> let myTime = 2 # time
< ghci> let myVelocity = myLength /# myTime
< ghci> myVelocity
< 2.5 m/s

New units are created "on demand". Furthermore

< ghci> let velocity = myVelocity
< ghci> let otherVelocity = 188 # velocity

it's possible to use the sugar from before on user-defined units.

TODO: Dessa har alla Double som värdetyp. Hur förhindra det? Explicita typsignaturer löser det, men man vill inte att "användaren" ska *behöva* och *få* göra det för att behålla den tidigare nämnda invarianten.

Just for convenience sake we'll define a bunch of common composite units.

> velocity     = length   /# time
> acceleration = velocity /# time
> force        = mass     *# acceleration
> momentum     = force    *# time

A physics example
-----------------

To conclude this chapter, we show an example on how to code an exercise and its solution in this domain specific language we've created for quantities.

The code comments show what GHCi prints for that row.

The exercise is "A dog runs, jumps and lands sliding on a carriage. The dog weighs `40 kg` and runs in `8 m/s`. The carriage weighs `190 kg`. The coefficient of friction between the dog's paws and the carriage is `0.7`."

This is illustrated in the painting below.

<img src="Exercise.png" alt="" style="width: 600px;"/>

a) Calculate the (shared) final velocity of the dog and the carriage.

> mDog      = 40 # mass    -- 40 kg
> viDog     = 8 # velocity -- 8 m/s
> mCarriage = 190 # mass   -- 190 kg
> u         = 0.7 # one    -- 0.7

No external forces are acting on the dog and the carriage. Hence the momentum of the system is preserved.

> piSystem = mDog *# viDog -- 320 kg*m/s
> pfSystem = piSystem      -- 320 kg*m/s

In the end the whole system has a shared velocity of its shared mass.

> mSystem  = mDog +# mCarriage   -- 230 kg
> vfSystem = pfSystem /# mSystem -- 1.39 m/s

b) Calculate the force of friction acting on the dog.

> fFriction = u *# fNormal        -- 275 kg*m/s^2
> fNormal   = mDog *# g           -- 393 kg*m/s^2
> g         = 9.82 # acceleration -- 9.82 m/s^2

c) For how long time does the dog slide on the carriage?

> aDog = fFriction /# mDog -- 6.87 m/s^2

< vDelta = mDog -# vfSystem

    * Couldn't match type 'Numeric.NumType.DK.Integers.Neg1
                     with 'Numeric.NumType.DK.Integers.Zero
    ...

Whoops! That's not a good operation. Luckily the compiler caught it.

> vDelta = viDog -# vfSystem -- 6.60 m/s
> tSlide = vDelta /# aDog    -- 0.96 s
