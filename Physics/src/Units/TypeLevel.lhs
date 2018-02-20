
Type-level units
================

We will now implement *type-level* units. What is type-level? When one usually programs (in Haskell), one operatates (e.g. adds) on values (e.g. `1` and `2`). This is on *value-level*. Now we will do the same thing but on *type-level*, that is, perform operations on types.

What's the purpose of type-level units? It's so that we'll notice as early as compile-time if we've written something incorrect.

As previously mentioned, this implementation will be very similar to that on value-level.

To be able to do type-level programming, we'll need a nice stash of GHC-extensions. TODO: förklara vad de gör.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE TypeOperators #-}

> module Units.TypeLevel
> ( Unit(..)
> , Mul
> , Div
> , Length
> , Mass
> , Time
> , Current
> , Temperature
> , Substance
> , Luminosity
> , One
> )
> where

We'll need to be able to operate on integers on type-level. Instead of implementing it ourselves, we will just import the machinery so we can focus on the physics-part.

> import Numeric.NumType.DK.Integers

We make a *kind* for units, just like we in the previous section made *type* for units. On value-level we made a *type* with *values*. Now we make a *kind* with *types*. The meaning is exactly the same, except we have moved "one step up".

> data Unit = Unit TypeInt -- Length
>                  TypeInt -- Mass
>                  TypeInt -- Time
>                  TypeInt -- Current
>                  TypeInt -- Temperature
>                  TypeInt -- Substance
>                  TypeInt -- Luminosity

But `data Unit = ...` looks awfully similar to a regular data type! That's correct. But with the GHC-extension `DataKinds` this will, apart from creating a regular data type, **also** create a *kind*. Perhaps a less confusing syntax would've been `kind Unit = ...`.

Thanks to the `Unit`-kind we can force certain types in functions to be of this kind.

This may sound confusing, but the point of this will become clear over time. Let's show some example *types* of the `Unit`-kind.

> type Length      = 'Unit Pos1 Zero Zero Zero Zero Zero Zero
> type Mass        = 'Unit Zero Pos1 Zero Zero Zero Zero Zero
> type Time        = 'Unit Zero Zero Pos1 Zero Zero Zero Zero
> type Current     = 'Unit Zero Zero Zero Pos1 Zero Zero Zero
> type Temperature = 'Unit Zero Zero Zero Zero Pos1 Zero Zero
> type Substance   = 'Unit Zero Zero Zero Zero Zero Pos1 Zero
> type Luminosity  = 'Unit Zero Zero Zero Zero Zero Zero Pos1
> 
> type Velocity     = 'Unit Pos1 Zero Neg1 Zero Zero Zero Zero
> type Acceleration = 'Unit Pos1 Zero Neg2 Zero Zero Zero Zero
> 
> type One = 'Unit Zero Zero Zero Zero Zero Zero Zero

`'Unit` is used to distinguish between the *type* `Unit` (left-hand-side of the `data Unit` definition) and the *type constructor* `Unit` (right-hand-side of the `data Unit` definition, with `DataKinds`-perspective). `'Unit` refers to the type constructor. Both are created when using `DataKinds`.

`Pos1`, `Neg1` and so on corresponds to `1` and `-1` in the imported package, which operates on integers on type-level.

Let's implement multiplication and division on type-level. After such an operation a new unit is created. And from the previous section we already know what the unit should look like. To translate to the Haskell-language: "after such an operation a new *type* is created". How does one implement that? With `type family`! `type family` can easiest be thought of as a function on the type-level.

> type family Mul (u1 :: Unit) (u2 :: Unit) where
>   Mul ('Unit le1 ma1 ti1 cu1 te1 su1 lu1) 
>       ('Unit le2 ma2 ti2 cu2 te2 su2 lu2) =
>       'Unit (le1+le2) (ma1+ma2) (ti1+ti2) (cu1+cu2)
>         (te1+te2) (su1+su2) (lu1+lu2)

- `type family` means that it's a function on type-level.
- `Mul` is the name of the function.
- `u1 :: Unit` is read as "the *type* `u1` has *kind* `Unit`".

Division is very similar.

> type family Div (u1 :: Unit) (u2 :: Unit) where
>   Div ('Unit le1 ma1 ti1 cu1 te1 su1 lu1) 
>       ('Unit le2 ma2 ti2 cu2 te2 su2 lu2) =
>       'Unit (le1-le2) (ma1-ma2) (ti1-ti2) (cu1-cu2)
>         (te1-te2) (su1-su2) (lu1-lu2)

Let's create some example *types* for units with multiplication and division.

> type Velocity' = Length `Div` Time
> type Area      = Length `Mul` Length
> type Force     = Mass   `Mul` Length
> type Impulse   = Force  `Mul` Time

Perhaps not very exiting so far. But just wait 'til we create a data type for quantities. Then the strenghts of type-level units will be clearer.
