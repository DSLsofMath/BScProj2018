
WIP

Kinematics
==========

Let's get into some kinematics shall we? In this chaper we'll define the kinematic formulas unambgiously and then encode them in Haskell.

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}

> module Proofs.Kinematics
> ( Expr(..)
> , Equal(..)
> )
> where

Rigorous names and defining equlities
-------------------------------------

You already know what forumulas we'll work with, but here they are anyway

\begin{align}
  v_f &= v_i + a*t \\
  x_f &= x_i + \frac{v_f + v_i}{2}*t \\
  x_f &= x_i + v_i*t + \frac{a*t^2}{2} \\
  v_f^2 &= v_i^2 + 2*a*(x_f - x_i) \\
\end{align}

What do all the names in the formulas *really* mean? In order to prove them we must first define very clearly what they mean. In fact, this first step involves no Haskell at all! It's all about clearing out ambiguoity. Have a look at this picture

![Overview](Overview.png)

We'll think in terms of a moving box when defining the names in the forumlas. The box has different *positions*, *velocities* and *accelerations* at different *points of time*. Therefore, $x(t)$, $v(t)$ and $a(t)$ will be the *current* position, velocity and acceleration, respectively, in a *certain* point of time $t$. $t$ is a "time index", marking a certain point in time.

"Final", "initial" and "0" refers to *specific* points of time in the experiment. Initial and 0 on the, dun dun dun, initial (!) point of time of the experiment and final on the final. These are *fix* points of time. If you only see just $f$ or just $i$, the refer to the final respective initial *state*. The context will tell if the state is a position, velocity, acceleration or time.

This reasoning gives the following *defining* equalities

\begin{align}
  t_f &= \{\text{Time at final, when the experiment is over}\} \\
  t_i &= \{\text{Time at initial, when the experiment starts}\} = t_0 = 0\\
  x_f &= x(t_f) \\
  x_i &= x(t_i) = x_0 \\
  v_f &= v(t_f) \\
  v_i &= v(t_i) = v_0 \\
\end{align}

They are defining because they *define* what the names actually mean.

You might wonder about that $t_0 = 0$ equiality. This is a convention to do. It simply means that the starting point of the experiment is set as the reference point of time.

Why is the acceleration not included? Well, these four forumlas are only valid when the *acceleration is constant*. Is $a$ constant? What does just $a$ even refer to? In this context, more explicitly, one means

\begin{align}
  a(t) = a_{value}
\end{align}

where $a_{value}$ is a *value*, a number. Now it becomes clear why $a_f$ and $a_i$ aren't included. They would simply be the same.

What about $\Delta$? The definition of $\Delta$ is a *change* in time/position/velocity. Change between what? At least it's a difference. Difference between what? Well, that is often not so clear. However, here we'll give it a clear definition, namely the difference between *current* and *initial*. This leads to

\begin{align}
  \Delta t &= t - t_i \\
  (\Delta x)(t) &= x(t) - x_i \\
  (\Delta v)(t) &= v(t) - v_i \\
\end{align}

$\Delta$ of something becomes a function of time. We wrote $(\Delta x)(t)$ and not $\Delta x(t)$. This is to emphasize that $\Delta$ refers to the *qunantity* and not the *function* describing the quantity. By having the $(t)$ outermost too, it becomes even more clear that the whole thing is a function.

There are two other equalities for $\Delta$, namely

\begin{align}
  (\Delta x)(t) &= \int_{t_i}^t v(t') dt' \\
  (\Delta v)(t) &= \int_{t_i}^t a(t') dt' \\
\end{align}

which is possible to understand given this picture

![Velocity integral](velocity_integral.png)

The distance travelled from $t_i$ to currently, $t$, can be determined if we know the velocity at all times during that period. To get the distance we integrate the velocity during the period.

Also note the $'$ in the integral. That's because $t$ in $(t)$ and as the upper limit *are not the same* as the "t" inside the integral.

These two equaltities are the *only ones* relating position to velocity and velocity to acceleration. They are axioms here. Without any such relation, we wouldn't come very far. Note that they are equivalent to

\begin{align}
  v(t) &= \frac{dx(t)}{dt} \\
  a(t) &= \frac{dv(t)}{dt} \\
\end{align}

We use to first pair instead because they are easier to work with.

Encoding the names and equalities
---------------------------------

As we saw, there were two components: *expressions* and *equalities*. Examples of expressions were $x(t)$, $v_f$ and $\int$. Examples of *equalities* were $t_i = t_0$ and $v(t_i) = v_i$. An equality is a statement because it claims something. And from Curry-Howard we know that statements are types. Hence equalities needs to be types. The equalities relate two expressions. Hence expressions needs to be types as well.

Expressions will be written in the following fashion

< data Expr = bla
<           | blah
<           | blahh           

which combined with the `DataKinds` extensions makes `Expr` and *kind* and whatever different expressions we have *types*. Just what we wanted!

Equalities will be written as

< data Equal (a :: Expr) (b :: Expr) where
<   Eq1 :: Equal bla blah
<   Eq2 :: Equal blah blah

that is, as a *GADT*. `Equal x y` is a type that says that the *expressions* `x` and `y` are equal. Values can be created by using and combining the constructors of the `Equal` type, here `Eq1` and `Eq2`.

Expressions
-----------

We'll begin by creating the expressions we need. For starters, there's addition, subtraction, multiplication and division between *other* expressions.

> data Expr = Expr `Add` Expr
>           | Expr `Sub` Expr
>           | Expr `Mul` Expr
>           | Expr `Div` Expr

The integral of an expression is also an expression.

>           | Integ Expr -- The expression to integrate
>                   Expr -- The lower bound
>                   Expr -- The upper bound
>                   Expr -- What to integrate with respect to

The main sort of expressions is however the symbolic names that were defined two chapters ago. There are the functions denoting the position, velocity and acceleration.

>           | Xfun Expr
>           | Vfun Expr
>           | Afun Expr

These types have an argument, since they are *functions*. The argument is another expression for the time they are to be evaluated in.

We also have the different initial and final values.

>           | X0
>           | Xi
>           | Xf
>           | V0
>           | Vi
>           | Vf
>           | T0
>           | Ti
>           | Tf

The value of the acceleration is another symbolic name.

>           | Avalue

And also the $\Delta$-functions.

>           | DeltaTfun Expr
>           | DeltaXfun Expr
>           | DeltaVfun Expr

Two numbers we'll need are $0$ and $2$, so let's include them here.

>           | Zero
>           | Two

And finally, a *polynomial function*

>           | PolyFun Expr -- a0
>                     Expr -- a1
>                     Expr -- a2
>                     Expr -- t

with the semantic meaning of

\begin{align}
  p(t) = a0 + a1 * t + a2 * t^2
\end{align}

What's the purpose of this? We'll need to evaluate integrals and as we'll se, by only allowing evalution of the one form, we can avoid cheating.

Defining equalities
-------------------

That's the all the expressions we needed. Now let's encode the equaltities that we need. There are two sorts of equalities. The defining ones, which define what a symbolic name mean, and mathematical equalities, for instance that addition is commutative.

> data Equal (x :: Expr) (y :: Expr) where

Among the defining equaltities we have those relating the initial and final states.

>   Xinitial1 :: Xi `Equal` Xfun Ti
>   Xinitial2 :: X0 `Equal` Xfun Ti
>   Xfinal    :: Xf `Equal` Xfun Tf
>   Vinitial1 :: Vi `Equal` Vfun Ti
>   Vinitial2 :: V0 `Equal` Vfun Ti
>   Vfinal    :: Vf `Equal` Vfun Tf
>   Tinitial1 :: Ti `Equal` T0
>   Tinitial2 :: Ti `Equal` Zero

We have the definitions of the $\Delta$-functions

>   DeltaXdef :: DeltaXfun t `Equal` (Xfun t `Sub` Xi)
>   DeltaVdef :: DeltaVfun t `Equal` (Vfun t `Sub` Vi)
>   DeltaTdef :: DeltaTfun t `Equal` (t `Sub` Ti)

and the relations between the $\Delta$-functions and integrals.

>   DeltaXint :: DeltaXfun t `Equal` Integ (Vfun t') Ti t t'
>   DeltaVint :: DeltaVfun t `Equal` Integ (Afun t') Ti t t'

Finally we have the equalitity for the constant acceleration

>   AfunCon :: Afun t `Equal` Avalue

som säger att funktionen för acceleration, *för alla* `t`, är lika med `Avalue`.

Matematiska likheter
--------------------

Okej, så nu har vi de definerande likheterna. Vi behöver även några matematiska likheter. Man skulle kunna tänka sig att definera *alla* som finns, bara för att det inte ska verka som att vi plockar ut det vi behöver, men det skulle bli väldigt många. Så vi låtsas vara lata och bara definerar några som råkar vara de vi behöver.

Är detta inte fusk? I vanlig bevisföring i datorn nöjer man sig med en enda grundläggande likhet `Refl` och bevisar *allt* utifrån den. Det skulle här vara "out-of-scope". Vi skaffar kraftfulla axiom så att vi kan fokusera på de bevis som görs i fysiken. Axiomen kommer "uppenbarligen" vara rätt.

De vi behöver blir...

Egenskaper hos likhet

>   Reflexive    :: a `Equal` a
>   Symmetry     :: a `Equal` b -> b `Equal` a
>   Transitivity :: a `Equal` b -> b `Equal` c -> a `Equal` c

Canceling out

>   MulDiv       :: ((b `Div` a) `Mul` a) `Equal` b
>   AddSub       :: ((b `Sub` a) `Add` a) `Equal` b

Kongruenser

>   CongAddL     :: a `Equal` b -> (a `Add` c) `Equal` (b `Add` c)
>   CongAddR     :: a `Equal` b -> (c `Add` a) `Equal` (c `Add` b)
>   CongSubL     :: a `Equal` b -> (a `Sub` c) `Equal` (b `Sub` c)
>   CongSubR     :: a `Equal` b -> (c `Sub` a) `Equal` (c `Sub` b)
>   CongMulL     :: a `Equal` b -> (a `Mul` c) `Equal` (b `Mul` c)
>   CongMulR     :: a `Equal` b -> (c `Mul` a) `Equal` (c `Mul` b)
>   CongInteg    :: a `Equal` b -> Integ a x y z `Equal` 
>                                  Integ b x y z
>   CongPoly2    :: a2 `Equal` a2' -> PolyFun a0 a1 a2 t `Equal`
>                                     PolyFun a0 a1 a2' t

Integraler

>   IntegEval    :: Integ (PolyFun a0   a1 Zero           t') l u t'
>                   `Equal`
>                        ((PolyFun Zero a0 (a1 `Div` Two) u) `Sub`
>                         (PolyFun Zero a0 (a1 `Div` Two) l))

Identiteter

> -- TODO: Bara L, bevisa R
>   ZeroNum      :: (Zero `Div` a) `Equal` Zero
>   ZeroMul      :: (Zero `Mul` a) `Equal` Zero
>   ZeroMulR     :: (a `Mul` Zero) `Equal` Zero
>   ZeroAddL     :: (Zero `Add` a) `Equal` a
>   ZeroAddR     :: (a `Add` Zero) `Equal` a
>   ZeroSub      :: (a `Sub` Zero) `Equal` a

Aritmetik

>   MulDistSub   :: (a `Mul` (b `Sub` c)) `Equal`
>                   ((a `Mul` b) `Sub` (a `Mul` c))
>   AddCom       :: (a `Add` b) `Equal` (b `Add` a)

Polynom

>   PolyEval     :: PolyFun a0 a1 a2 t `Equal` ((a0 `Add` (a1 `Mul` t)) `Add` (a2 `Mul` (t `Mul` t)))


































