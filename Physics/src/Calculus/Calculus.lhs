What is calculus?
======================================================================

TODO: Move relevant stuff out of the general structures/eval to their
      respective sections? (differences, derivatives, integrals)
      Or maybe, first introduce differences et al. separately, and then
      tie them together in a common section of evaluation?

TODO: Make the text gooder in general

TODO: Quotes -> good teaching text (especially in Integral secion)

TODO: Proofs/tests/verification

TODO: Improve DSLs a bit? I don't like the `Expr` tree very much.
      Separate into `FunExpr` and `Expr`, where `Expr` is very simple?

TODO: Have someone critique this

Plain equations where all values are of the same dimension are all
fine and well.  The importance of being able to solve basic problems
like "If Jenny has 22 meters, and Richard has 18 meters: how many
meters do they have together?" cannot be understated, but they're not
especially fun!

"An unstoppable car has an unchanging velocity of 141.622272
km/h. How many kilometers has it droven after a day?". To solve
more interesting problems like this, we need calculus.

Calculus is the study of stuff that continuously change over time (or
some other continuous variable). For example, a distance that changes
over time is equivalent to a speed or a velocity, depending on how
many dimensions you have, and a volume that changes as a length
changes does not have a name, as far as I know.

There are two major branches of calculus, differential calculus and
integral calculus. Differential calculus is all about those rates of
changes and graph slopes.  Differences, differentials, derivatives,
and the like. Integral calculus, on the other hand, is all about
accumulation and areas. Sums, integrals, and such.

In this chapter we'll expore the syntax of diffences, the problem with
differentials, symbolic differentiation, numeric and symbolic
integration, and some applied problem solving.



Boring boilerplate
----------------------------------------------------------------------

Firstly, let's get the boring stuff out of the way. This is our module!

> module Calculus.Calculus where

Important imports!

> import Data.Maybe
> import Data.List
> import Data.String
> import Control.Exception
> import Test.QuickCheck

This import is especially interesting.
[Hatlab](https://github.com/DSLsofMath/Hatlab) is a very simple graph
plotting library that we can use to draw pretty graphs of our
functions, derivatives, and integrals later!

> import Hatlab.Plot



Semantics, syntax, and lambda calculus
----------------------------------------------------------------------

What is a value in calculus? What kind of values do functions in
calculus operate on and produce?

Let's look at derivatives to get an idea of what the semantic value of calculus is.

$$\frac{d x^2}{dx} = 2x$$

$$\frac{d f(x)}{dx} = f'(x)$$

Hmm, these examples left me more confused than before. The
differentiation function seems to take an expression as an argument,
and return the derived expression, with regards to a variable. But
what is an expression represented as a semantic value? It's not a
number yet, the variable in the body needs to be substituted first in
order for the expression to be computable. Is it some kind of function
then? Well, yes it is! If we reinterpret the differentiation
expressions above, it makes more sense.

$$\frac{d x^2}{dx} = 2x$$

can be written as

$$D(x^2) = 2x \text{ with regards to } x$$

which is really equivalent to

$$D(x \mapsto x^2) = x \mapsto 2x$$

or

$$D(square) = double$$.

So the type of unary real functions seems like a great fit for a
semantic value for calculus, and it is! Great! But... how do we
represent a real number in Haskell? There is no `Real` type to
use. Well, for simplicitys sake we can just say that a real number is
basically the same as a `Double`, and it is (basically). The problem
with `Double` is that it's of finite precision, so rounding errors may
occur. We'll have to keep that in mind when doing calculations!

> type RealNum = Double
>
> -- The type of the semantic value of calculus is the unary real function
> --   RealNum -> RealNum

Now, to the syntax. We've concluded that real functions are really
what calculus is all about, so let's model them.

> data FunExpr

First of all, there's the elementary functions. We can't have them
all, that would get too repetitive to implement, but we'll put in all
the fun ones.

>     = Exp
>     | Log
>     | Sin
>     | Cos
>     | Asin
>     | Acos

Then, there are the arithmetic operators. "But wait", you say, "Aren't
arithmetic operators used to combine expressions, not functions?". I
hear you, Billy, but we will do it anyways. We could make a `Lambda`
constructor for "VAR $\mapsto$ EXPR" expressions and define the
arithmetic operators for the expression type, but this would make
our language much more complicated! Instead, we'll restrain ourselves
to single variable expressions, which can be represented as
compositions of unary functions, and define the arithmeric operators
for the functions instead.

$$f \text{ OP } g = x \mapsto (f(x) \text{ OP } g(x))$$

>     | FunExpr :+ FunExpr
>     | FunExpr :- FunExpr
>     | FunExpr :* FunExpr
>     | FunExpr :/ FunExpr
>     | FunExpr :^ FunExpr

And then theres that single variable. As everything is a function
expression, the function that best represents "just a variable" would
be $x \mapsto x$, which is the same as the $id$ function.

>     | Id

In a similar vein, the constant function. $const(c) = x \mapsto c$

>     | Const RealNum

Then theres function composition. If you didn't already know it, it's
defined as

$$f . g = x \mapsto f(g(x))$$

>     | FunExpr :. FunExpr

Finally, the real heroes: The functions of difference, differentiation,
and integration! They will be well explored later. But for now, we
define the syntax for them as

>     | Delta RealNum FunExpr
>     | D FunExpr
>     | I RealNum FunExpr

Even more finally, we add a `deriving` modifier to automatically allow
for equality tests between `FunExpr`s.

>   deriving Eq

Nice! This syntax tree will allow us to do symbolically (at the syntax
level) what we otherwise would have to do numerically (at the
semantics level).

Before we move on, we just have to fix one thing: the operator
precedence! If we don't do anything about it, this will happen

< ghci> Id :+ Id :* Id == (Id :+ Id) :* Id
< True

Now this is obviously wrong. *Plus* doesn't come before *times*,
unless I accidentaly switched timelines in my sleep. To fix this, we
have to fix the fixity. `infixl` allows us to make an operator
left-associative, and set the precedence.

> -- Medium precedence
> infixl 6 :+
> infixl 6 :-
> -- Higher
> infixl 7 :*
> infixl 7 :/
> -- Higherer
> infixl 8 :^
> -- High as a kite
> infixl 9 :.



A structure with class
----------------------------------------------------------------------

Now that we've defined the basic structure of our language, we can
instantiate some useful classes. There are two in particular we care
for: `Show` and `Arbitrary`.

Try modifying `FunExpr` to derive `Show`, so that our expressions can be printed.

<   deriving Eq, Show

Consider now how GHCI prints out a function expression we create

< ghci> carAccel = Const 20
< ghci> carSpeed = Const 50 :+ carAccel :* Id
< ghci> carPosition = Const 10 :+ carSpeed :* Id
< ghci> carPosition
< Const 10.0 :+ (Const 50.0 :+ Const 20.0 :* Id) :* Id

Well that's borderline unreadable. Further, the grokability of a printed expression is very inversely proportional to the size/complexity of the expression, as I'm sure you can imagine.

So if the `Show` is bad, we'll just have to make our own `Show`!

> instance Show FunExpr where
>   show Exp = "exp"
>   show Log = "log"
>   show Sin = "sin"
>   show Cos = "cos"
>   show Asin = "asin"
>   show Acos = "acos"
>   show (f :+ g) = "(" ++ show f ++ " + " ++ show g ++ ")"
>   show (f :- g) = "(" ++ show f ++ " - " ++ show g ++ ")"
>   show (f :* g) = "(" ++ show f ++ " * " ++ show g ++ ")"
>   show (f :/ g) = "(" ++ show f ++ " / " ++ show g ++ ")"
>   show (f :^ g) = "(" ++ show f ++ "^" ++ show g ++ ")"
>   show Id = "id"
>   show (Const x) = showReal x
>   show (f :. g) = "(" ++ show f ++ " . " ++ show g ++ ")"
>   show (Delta h f) = "(delta_" ++ showReal h ++ " " ++ show f ++ ")"
>   show (D f) = "(D " ++ show f ++ ")"
>   show (I c f) = "(I at " ++ show c ++ " for " ++ show f ++ ")"
>
> showReal x = if isInt x then show (round x) else show x
>   where isInt x = x == fromInteger (round x)

Not much to explain here. It's just one way to print our syntax tree
in a more readable way. What's interesting is how we can now print our
expressions in a much more human friendly way!

< ghci> carPosition
< (10 + ((50 + (20 * id)) * id))

Still a bit noisy with all the parens, but much better!

Another class we need to instance for our `FunExpr` is
`Arbitrary`. This class is associated with the testing library
*QuickCheck*, and describes how to generate arbitrary values of a type
for use when testing logical properties with `quickCheck`. For
example, a property function could be formulated that states that the
`:*` constructor of `FunExpr` is associative.

The implementation itself is not very interesting. We generate a
function expression that tends to contain mostly elementary functions,
arithmetic operations, and a generous dose of constants; with a light
sprinkle of differences, derivatives, and integrals.

> instance Arbitrary FunExpr where
>   arbitrary =

`frequency` "chooses one of the given generators, with a weighted
random distribution". By assigning probabilities of generating certain
functions more often than others, we can restrain the growth of the
generated expressions in complexity.

>       frequency
>         [ (10, genElementary)
>         , (10, genBinaryOperation)
>         , (10, return Id)
>         , (20, fmap Const arbitrary)
>         , (10, genBinaryApp (:.))
>         , (5 , genBinaryApp Delta)
>         , (5 , fmap D arbitrary)
>         , (5 , genBinaryApp I) ]
>     where genElementary = elements [Exp, Log, Sin, Cos, Asin, Acos]
>           genBinaryApp op = fmap (\(f, g) -> f `op` g) arbitrary
>           genBinaryOperation =     elements [(:+), (:-), (:*), (:/), (:^)]
>                                >>= genBinaryApp



Deep, dark, differences
----------------------------------------------------------------------

![](delta.png "Feel the might if the illum-... the delta!"){.float-img-left}

A *difference* is, in it's essence, quite simply the result of
applying the operation of subtraction to two real number terms.

$$minuend - subtrahend = difference$$

Nice, great job, we're done here, let's move on.

...

Just kidding, of course there's more to it than that.

In calculus, the term *difference* carries more meaning than
usual. More than just a subtraction of arbitrary values, differences
lie at the heart of calculations regarding rate of change, both
average and instantaneous.

Quotients of differences of functions of the same time describe the
average rate of change over the time period. For example, an average
velocity can be described as the difference quotient of difference in
position divided by difference in time.

$$v_{avg} = \frac{p_2 - p_1}{t_2 - t_1}$$  where $p_n$ is the position
at time $t_n$.

In the context of calculus, we use a special syntax for differences:
the delta operator! With this, the previous definition can be
rewritten as

$$v_{avg} = \frac{p_2 - p_1}{t_2 - t_1} = \frac{\Delta p}{\Delta t}$$.

This is the informal definition of the delta operator used in *University Physics*:

$$ \Delta x = x_2 - x_1 $$

Ok, so it's a difference. But what does $x_2$ and $x_1$ mean, and what do they come from?
$x_2$ and $x_1$ are not explicitly bound anywhere,
but seems reasonable to assume that $x_i \in x$ or equivalently, that $x$
is a function with a subscript index as an argument, that returns a $\mathbb{R}$.

Further, the indices $1,2$ should not be thought of as specific constants,
but rather arbitrary real number variables identified by these integers.
Lets call them $a,b$ instead, to make it clear that they are not constants.

$$\Delta x = x_b - x_a$$

Now $a,b$ are implicitly bound. We make the binding explicit.

$$ (\Delta x)(a, b) = x_b - x_a $$

We compare this to the more formal definition of *forward difference*
on wikipedia:

$$\Delta_h[f](x) = f(x + h) - f(x)$$

The parameter bindings are a bit all over the place here. To more easily compare
to our definition, let's rename $x$ to $a$ and $f$ to $x$, and change the parameter
declaration syntax:

$$ (\Delta x)(h)(a) = x(a + h) - x(a) $$

This is almost identical to the definition we arrived at earlier, with
the exception of expressing $b$ as $a + h$. Why is this? Well, in
calculus we mainly use differences to express two things, as mentioned
previously. Average rate of change and instantaneous rate of change.

Average rate of change is best described as the difference quotient of
the difference in y-axis value over an interval of x, divided by the
difference in x-axis value over the same interval.

$$\frac{y(x_b) - y(x_a)}{x_b - x_a}$$.

In this case, the $x$'s can be at arbitrary points on the axis, as
long as $b > a$. Therefore, the definition of difference as $(\Delta
x)(a, b) = x_b - x_a$ seems a good fit. Applied to average velocity, our difference quotient

$$v_{avg} = \frac{\Delta p}{\Delta t}$$

will expand to

$$v_{avg}(t_2, t_1) = \frac{(\Delta p)(t_2, t_1)}{(\Delta t)(t_2, t_1)}$$ for $t_2 > t_1$.

Instantaneous rate of change is more complicated. At its heart, it
too is defined in terms of differences. However, we are no longer
looking at the average change over an interval delimited by two
points, but rather the instantaneous change in a single point.

Of course, you can't have a difference with only one point. You need
two points to look at how the function value changes between them. But
what if we make the second point reeeeeeeeealy close to the first?
That's basically the same as the difference in a single point, for all
intents and purposes. And so, for instantaneous rate of change, the
definition of difference as $(\Delta x)(h)(a) = x(a + h) - x(a)$ will
make more sense, for very small $h$'s. Applied to instantaneous
velocity, our difference quotient

$$v_{inst} = \frac{\Delta p}{\Delta t}$$

for very small $\Delta t$, will expand to

$$v_{inst}(h, x) = \frac{(\Delta p)(h, x)}{(\Delta t)(h, x)}$$

for very small $h$.

As $h$ gets closer to $0$, our approximation of instantaneous rate of
change gets better.

And so, we have a method of computing average rate of change, and
instantaneous rate of change (numerically approximatively). In
Haskell, we can make shallow embeddings for differences in the context
of rate of change as velocity.

Average velocity is simply

> v_avg pos t2 t1 = (pos(t2) - pos(t1)) / (t2 - t1)

which can be used as

< ghci> v_avg (\x -> 5*x) 10 0
< 5.0

And instantaneous velocity is

> v_inst pos h t = (pos(t + h) - pos(t)) / ((t + h) - t)

which can be used as

< ghci> carSpeed t = v_inst (\x -> x + sin(x)) 0.00001 t
< ghci> carSpeedAtPointsInTime = map carSpeed [0, 1, 2, 3]
< ghci> carSpeedAtPointsInTime
< [1.9999999999833333,1.5402980985023251,0.5838486169602084,1.0006797790330592e-2]

We'd also like to model one of the versions of the delta operator, finite difference, in our syntax tree. As the semantic value of our calculus language is the unary real function, the difference used for averages doesn't really fit in well, as it's a binary function (two arguments: $t_2$ and $t_1$). Instead, we'll use the version of delta used for instantants, as it only takes a single point in time as an argument (assuming $h$ is already given).

The constructor in our syntax tree is therefore

<     | Delta RealNum FunExpr

where the first argument is $h$, and the second is the function.



Derivatives
======================================================================

The *derivative* of a function is, according to wikipedia, "the slope
of the tangent line to the graph of [a] function at [a] point" and can
be described as the "instantaneous rate of change", and
*differentiation* is the method of finding a derivative for a
function.

...

Wait, didn't we just look at instantaneous rates of changes (blarh my
tounge is getting tired) in the previous section on differences? Well
yes, and the difference quotient for a function at a point with a very
small step $h$ is indeed a good way to numerically approximate the
derivative of a function. From what we found then, we can derive a
general expression for instantaneous rate of change

$$\frac{(\Delta f)(h, x)}{(\Delta id)(h, x)} = \frac{f(x + h) - f(x)}{h}$$

for very small $h$.

But what if we don't want just a numerical approximation, but THE
derivative of a function at any arbitrary point? What if we make $h$ not just very small, but *infinitley* small?

Introducing *infinitesimals*! From the wikipedia entry on *Leibniz's notation*

 > In calculus, Leibniz's notation, named in honor of the 17th-century
 > German philosopher and mathematician Gottfried Wilhelm Leibniz,
 > uses the symbols $dx$ and $dy$ to represent infinitely small (or
 > infinitesimal) increments of x and y, respectively, just as $\Delta x$ and
 > $\Delta y$ represent finite increments of x and y, respectively.

So there's a special syntax for differences where the step $h$ is
infinitely small, and it's called Leibniz's notation. We interpret the
above quote in mathematical terms:

$$dx = lim_{\Delta x \to 0} \Delta x$$

such that

$$\forall y(x). D(y) = \frac{dy}{dx} = \frac{lim_{\Delta y \to 0} \Delta y}
                                            {lim_{\Delta x \to 0} \Delta x}$$

where $D$ is the function of differentiation.

This definition of derivatives is very appealing, as it suggests a
very simple and intuitive transition from finite differences to
infinitesimal differentials. Also, it suggests the possibility of
manipulating the infinitesimals of the derivative algebraically, which
might be very useful. However, this concept is generally considered
too imprecise to be used as the foundation of calculus.

A later section on the same wikipedia entry elaborates a bit:

 > Leibniz's concept of infinitesimals, long considered to be too
 > imprecise to be used as a foundation of calculus, was eventually
 > replaced by rigorous concepts developed by Weierstrass and
 > others. Consequently, Leibniz's quotient notation was re-interpreted
 > to stand for the limit of the modern definition. However, in many
 > instances, the symbol did seem to act as an actual quotient would and
 > its usefulness kept it popular even in the face of several competing
 > notations.

What is then the "right" way to do derivatives? As luck would have it, not much differently than Leibniz's suggested! The intuitive idea can be turned into a precise definition by defining the derivative to be the limit of difference quotients of real numbers. Again, from wikipedia - Leibniz's notation:

 > In its modern interpretation, the expression dy/dx should not be read
 > as the division of two quantities dx and dy (as Leibniz had envisioned
 > it); rather, the whole expression should be seen as a single symbol
 > that is shorthand for
 >
 > $$D(x) = lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x}$$

which, when $y$ is a function of $x$, and $x$ is the $id$ function for real numbers (which it is in the case of time), is:

\begin{align*}
D(y) &= lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x} \\
     &= a \mapsto lim_{\Delta x \to 0} \frac{(\Delta y)(\Delta x, a)}{\Delta x} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + (\Delta x)(h, a)) - y(a)}{(\Delta x)(h, a)} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + ((a + h) - a)) - y(a)}{(a + h) - a} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + h) - y(a)}{h}
\end{align*}

There, the definition of derivatives! Not to complicated, was it?

The differentiation function is represented in our syntax tree as

<     | D FunExpr

Very simple!

And so, now what? What was the point of deriving that fancy definition
for derivatives? Well, now we can derive things symbolically, which
implies provable 100% perfect accuracy, no numeric approximations!

We define a function to symbolically derive a function
expression. `derive` takes a function expression, and returns the
derived function expression.

> derive :: FunExpr -> FunExpr

Using only the definition of derivatives, we can derive the
definitions of `derive` for the various constructors in our syntax tree.

For example, how do we derive `f :+ g`? Let's start by doing it
mathematically.

\begin{align*}
D(f + g) &= a \mapsto lim_{h \to 0} \frac{(f + g)[a + h] - (f + g)[a]}{h} \\
         & \text{ \{ Addition of functions \} } \\
         &= a \mapsto lim_{h \to 0} \frac{f(a + h) + g(a + h) - (f(a) + g(a))}{h} \\
         &= a \mapsto lim_{h \to 0} \frac{f(a + h) + g(a + h) - f(a) - g(a)}{h} \\
         &= a \mapsto lim_{h \to 0} (\frac{f(a + h) - f(a)}{h} + \frac{g(a + h) - g(a)}{h}) \\
         &= a \mapsto ((lim_{h \to 0} \frac{f(a + h) - f(a)}{h}) + (lim_{h \to 0} \frac{g(a + h) - g(a)}{h})) \\
         &= (a \mapsto lim_{h \to 0} \frac{f(a + h) - f(a)}{h}) + (a \mapsto lim_{h \to 0} \frac{g(a + h) - g(a)}{h}) \\
         & \text{ \{ Definition of derivative \} } \\
         &= D(f) + D(g)
\end{align*}

Oh, it's just the sum of the derivatives of both functions! The
Haskell implementation is then trivially

> derive (f :+ g) = derive f :+ derive g

Let's do one more, say, $sin$. We will make use of the trigonometric
identity of sum-to-product

$$\sin \theta - \sin \varphi = 2 \sin\left(\frac{\theta - \varphi}{2}\right) \cos\left(\frac{\theta + \varphi}{2}\right)$$

And the limit

$$\lim_{x \to 0} \frac{sin x}{x} = 1$$

which can be proved using the unit circle and squeeze theorem, but we
won't do that here.

Then, the differentiation

\begin{align*}
D(sin) &= a \mapsto lim_{h \to 0} \frac{sin(a + h) - sin(a)}{h} \\
       & \text{ \{ trig. sum-to-product \} } \\
       &= a \mapsto lim_{h \to 0} \frac{2 \sin\left(\frac{a + h - a}{2}\right) \cos\left(\frac{a + h + a}{2}\right)}{h} \\
       &= a \mapsto lim_{h \to 0} \frac{2 \sin\left(\frac{h}{2}\right) \cos\left(\frac{2a + h}{2}\right)}{h} \\
       &= a \mapsto lim_{h \to 0} \frac{2 \sin\left(\frac{h}{2}\right) \cos\left(\frac{2a + h}{2}\right)}{h} \\
       &= a \mapsto lim_{h \to 0} \frac{\sin\left(\frac{h}{2}\right)}{\frac{h}{2}} \cos\left(\frac{2a + h}{2}\right) \\
       & \text{\{} h \text{ approaches } 0 \text{\}} \\
       &= a \mapsto 1 \cos\left(\frac{2a + 0}{2}\right) \\
       &= a \mapsto \cos(a) \\
       &= \cos \\
\end{align*}

Again, trivial definition in Haskell

> derive Sin = Cos

I'll leave the proving of the rest of the implementations as an exercise to you, the reader.

> derive Exp = Exp
> derive Log = Const 1 :/ Id
> derive Cos = Const 0 :- Sin
> derive Asin = Const 1 :/ (Const 1 :- Id:^(Const 2)):^(Const 0.5)
> derive Acos = Const 0 :- derive Asin
> derive (f :- g) = derive f :- derive g
> derive (f :* g) = derive f :* g :+ f :* derive g
> derive (f :/ g) = (derive f :* g :- f :* derive g) :/ (g:^(Const 2))
> derive (f :^ g) = f:^(g :- Const 1) :* (g :* derive f :+ f :* (Log :. f) :* derive g)
> derive Id = Const 1
> derive (Const _) = Const 0
> derive (f :. g) = derive g :* (derive f :. g)
> derive (Delta h f) = Delta h (derive f)
> derive (D f) = derive (derive f)

Oh right, I almost forgot: Integrals. How are you supposed to know how
to derive these bad boys when we haven't even covered them yet! We'll
prove why this works later, but for now, just know that another name
for integral is *Antiderivative*...

> derive (I c f) = f



Keep it simple
----------------------------------------------------------------------

So we've got our differentiation function, great! Let's try it out by
finding the derivative for a simple function, like $f(x) = sin(x) +
x^2$, which should be $f'(x) = cos(x) + 2x$:

< ghci> f = Sin :+ Id:^(Const 2)
< ghci> derive f
< (cos + ((id^(2 - 1)) * ((2 * 1) + ((id * (log . id)) * 0))))

Oh... that's not very readable. If we simplify it manually we get that
the result is indeed as expected

< (cos + ((id^(2 - 1)) * ((2 * 1) + ((id * (log . id)) * 0))))
< cos + (id^1 * (2 + (id * (log . id) * 0)))
< cos + (id * (2 + 0))
< cos + 2*id

But still, we shouldn't have to do that manually! Let's have
Mr. Computer help us out, by writing a function to simplify
expressions.

We'll write a `simplify` function will reduce an expression to a
simpler, equivalent expression. Sounds good, only... what exactly does
"simpler" mean? Is $10$ simpler than $2 + 2 * 4$? Well, yes obviously,
but there are other expressions where this is not the case. For
example, polynomials have two standard forms. The first is the sum of
terms, which is appropriate when you want to add or subtract
polynomials. The other standard form is the product of irreducible
factors, which is a good fit for when you want to divide polynomials.

So, our `simplify` function will not guarantee that every expression
is reduced to *its most simple* form, but rather that many expressions
will be reduced to *a simpler form*. As an exercise, you can implement
more reduction rules to make expressions simpler to you. For example,
the trigonometric identities like $sin(\theta + \frac{\pi}{2}) =
cos(\theta)$.

> simplify :: FunExpr -> FunExpr

The elementary functions by themselves are already as simple as can
be, so we don't have to simplify those. When it comes to the
arithmetic operations, most interesting is the cases of one operand
being the identity element.

> simplify (f :+ g) = case (simplify f, simplify g) of
>     (Const 0, g') -> g'
>     (f', Const 0) -> f'
>     (Const a, Const b) -> Const (a + b)
>     (f', g') | f' == g' -> simplify (Const 2 :* f')
>     (Const a :* f', g') | f' == g' -> simplify (Const (a + 1) :* f')
>     (f', Const a :* g') | f' == g' -> simplify (Const (a + 1) :* f')
>     (Const a :* f', Const b :* g') | f' == g' -> simplify (Const (a + b) :* f')
>     (f', g') -> f' :+ g'
> simplify (f :- g) = case (simplify f, simplify g) of
>     (f', Const 0 :- g') -> f' :+ g'
>     (f', Const 0) -> f'
>     (Const a, Const b) -> if a > b then Const (a - b) else Const 0 :- Const (b - a)
>     (f', g') | f' == g' -> Const 0
>     (Const a :* f', g') | f' == g' -> simplify (Const (a - 1) :* f')
>     (f', Const a :* g') | f' == g' -> Const 0 :- simplify (Const (a - 1) :* f')
>     (Const a :* f', Const b :* g') | f' == g' -> simplify ((Const a :- Const b) :* f')
>     (f', g') -> f' :- g'
> simplify (f :* g) = case (simplify f, simplify g) of
>     (Const 0, g') -> Const 0
>     (f', Const 0) -> Const 0
>     (Const 1, g') -> g'
>     (f', Const 1) -> f'
>     (Const a, Const b) -> Const (a * b)
>     (f', Const c) -> Const c :* f'
>     (f', g') | f' == g' -> f' :^ Const 2
>     (Const a :* f', g') -> simplify (Const a :* (f' :* g'))
>     (f', Const a :* g') -> simplify (Const a :* (f' :* g'))
>     (f', g') -> f' :* g'
> simplify (f :/ g) = case (simplify f, simplify g) of
>     (Const 0, g') -> Const 0
>     (f', Const 1) -> f'
>     (f', g') | f' == g' -> Const 1
>     (f', g') -> f' :/ g'
> simplify (f :^ g) = case (simplify f, simplify g) of
>     (f', Const 1) -> f'
>     (f', g') -> f' :^ g'
> simplify (f :. g) = case (simplify f, simplify g) of
>     (Id, g') -> g'
>     (f', Id) -> f'
>     (f', g') -> f' :. g'
> simplify (Delta h f) = Delta h (simplify f)
> simplify (D f) = D (simplify f)
> simplify (I c f) = I c (simplify f)
> simplify f = f

With this new function, many expressions become much more readable!

< ghci> f = Sin :+ Id:^(Const 2)
< ghci> derive f
< (cos + ((id^(2 - 1)) * ((2 * 1) + ((id * (log . id)) * 0))))
< ghci> simplify (derive f)
< (cos + (2 * id))

A sight for sore eyes!



> {-



Integrals - An integral part of calculus
======================================================================

![](integral.png "A snaky integral"){.float-img-right}

*Integrals* are functions used to describe area, volume, and
accumulation in general. The operation of integration is the second
fundamental operation of calculus, and the inverse of
differentiation. Whereas derivatives are used to describe the rate of
change in an instant, integrals are used to describe the accumulation
of value over time.

Recall how we used derivatives before. If we know the distance
traveled of a car and the time it took, we can use differentiation to
calculate the velocity. Similarly but reversly, if we know the
velocity of the car and the time it travels for, we can use
integration to calculate the distance traveled.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$.

Ok, let's dive into this! We need to grok the syntax and find a
rigorous, modelable definition of what *exactly* an integral is. We
ask our kind friend Wikipedia for help. From the entry on *Integral*:

 > Given a function $f$ of a real variable $x$ and an interval $[a, b]$
 > of the real line, the definite integral
 >
 > $$\int_a^b f(x) dx$$
 >
 > ![A definite integral of a function can be represented as the signed area of the region bounded by its graph. (C) KSmrq](https://upload.wikimedia.org/wikipedia/commons/9/9f/Integral_example.svg){.float-img-left .img-border}
 > 
 > is defined informally as the signed area of the region in the
 > $xy$-plane that is bounded by the graph of $f$, the $x$-axis
 > and the vertical lines $x = a$ and $x = b$. The area above the
 > $x$-axis adds to the total and that below the $x$-axis subtracts
 > from the total.
 >
 > Roughly speaking, the operation of integration is the reverse of
 > differentiation. For this reason, the term integral may also refer to
 > the related notion of the antiderivative, a function $F$ whose
 > derivative is the given function $f$. In this case, it is called an
 > indefinite integral and is written:
 >
 > $$F(x) = \int f(x) dx$$

Ok, so first of all: confusion. Apparently there are two different
kinds of integrals, *definite integrals* and *indefinite integrals*?

Let's start with defining *indefinite* integrals. *Wikipedia -
Antiderivative* tells us that the *indefinite* integral, also known as
the *antiderivative*, of a function $f$ is equal to a differentiable
function $F$ such that $D(F) = f$. It further tells us that the
process of finding the antiderivative is called *antidifferentiation*
or *indefinite integration*.

The same article then brings further clarification

 > Antiderivatives are related to definite integrals through the
 > fundamental theorem of calculus: the definite integral of a
 > function over an interval is equal to the difference between the
 > values of an antiderivative evaluated at the endpoints of the
 > interval.

Ok, so indefinite integrals are the inverse of derivatives, and
definite integrals are just the application of an indefinite integral
to an interval. Good, that doesn't seem to complicated.



TODO: syntax analysis and modeling here

TODO: Also look at numerical computation of integrals

TODO: Then looking at the formal definition of integration to get `integrate`.




The heart of integrals and derivatives, the *fundememntal theorem of
calculus*, was independently discovered by both Newton and Leibniz.
They based their definitions on infinitesimals which, as described
earlier, was considered too imprecise. Later, Riemann rigorously
formalized integration using limits.

 > There are many ways of formally defining an integral, not all of
 > which are equivalent. The differences exist mostly to deal with
 > differing special cases which may not be integrable under other
 > definitions, but also occasionally for pedagogical reasons. The most
 > commonly used definitions of integral are Riemann integrals and
 > Lebesgue integrals.

We only look at Riemann integrals for now, since they will likely be more familiar to most students (They are to me!).

 > In the branch of mathematics known as real analysis, the Riemann
 > integral, created by Bernhard Riemann, was the first rigorous
 > definition of the integral of a function on an interval. It was
 > presented to the faculty at the University of Göttingen in 1854, but
 > not published in a journal until 1868.[1] For many functions and
 > practical applications, the Riemann integral can be evaluated by the
 > fundamental theorem of calculus or approximated by numerical
 > integration.
 >
 > The Riemann integral is unsuitable for many theoretical
 > purposes. Some of the technical deficiencies in Riemann integration
 > can be remedied with the Riemann–Stieltjes in > tegral, and most
 > disappear with the Lebesgue integral.

See: [Why should one still teach Riemann integration? (Mathoverflow)](https://mathoverflow.net/questions/52708/why-should-one-still-teach-riemann-integration)

So there are some problems, but Riemann integrals will do for now.(?)

% Because we won't do rigorous proofs requiring Lebesgue integrals in this course?

We will also only study definite integrals here, as indefinite integrals are not required to be used in this course anyway.

Geometrically (in 2D), the integral of a function over an interval is equivalent to the area under the graph of that function
over the same interval.

 > The definite integral is defined informally as the signed area of the
 > region in the xy-plane that is bounded by the graph of f, the x-axis
 > and the vertical lines $x = a$ and $x = b$. The area above the x-axis adds
 > to the total and that below the x-axis subtracts from the total.
 >
 > -- <cite>[Wikipedia - Integral](https://en.wikipedia.org/wiki/Integral)</cite>

This is the syntax for definite integrals

$$ \int_a^b f(x) dx $$

This would be read as "The signed area under the graph of $f(x)$ from $x=a$ to $x=b$".

Clearly, the integral symbol binds $a$ and $b$ which are then the limits of the integral, and constitute
the interval over which to integrate/take the area, and $f$ is the function which we integrate/take the area under.
The less obvious part of the syntax is the $(x)$ part in $f(x)$ and the $dx$. Is $x$ an argument to be passed to the
integral that is bound here? No. What this syntax actually implies is that, in english, "For every infinitesimal interval of $x$,
starting at $a$ and ending at $b$, take the value of $f$ at that x (equiv. to taking the value at any point in the infinitesimal
interval), and calculate the area of the rectangle with width $dx$ and height $f(x)$, then sum all of these parts together."

If we assume that $f$ is a unary function, which is the only kind of function we will be dealing with here, the syntax is redundant.
$dx$ is completely internal to the behaviour of the function, and is not an argument bound by it, we could easily omitt it from the syntax.
Further, if we see the integral as a function that integrates a function over an interval, $\int_a^b f(x)$ doesn't really make any sense, and
should just be $\int_a^b f$.

This syntax is much simpler and leaves less room for confusion.

`integrate f a b =` $\int_a^b f$

Now for the definition.

We look again at the informal definition above. $\int_a^b f$ is the sum of all rectangles with height $f(x)$
and width $dx$ for all infinitesimal intervals $dx$ between $a$ and $b$.

So we're dealing with an infinite sum of infinitesimal parts: a limit must be involved. $a$ and $b$ must be
the lower and upper limits of the sum. Our iteration variable should increase with infinitesimal $dx$ each step.
Each step we add the area of the rectangle with height $f(x')$, where $x'$ is any point in $[x$, $x + dx]$.
As $x + dx$ approaches $x$ when $dx$ approaches zero, $x' = lim_{dx \to 0} x + dx = x$.

$$ \int_a^b f = lim_{dx \to 0} \sum_{x = a, a + dx, a + 2dx, ...}^b A(x, dx) \text{ where } A(x, dx) = f(x) * dx $$

Based on this definition, we could implement a function in haskell to compute the numerical approximation of the integral
by letting $dx$ be a very small, but finite, number instead of being infinitesimal. The smaller our $dx$, the better the
approximation

Here's a pic that shows that smaller $dx$ results in better approximations:

![Riemann sum convergence, (C) KSmrq](https://upload.wikimedia.org/wikipedia/commons/2/2a/Riemann_sum_convergence.png){.img-border}

> integrateApprox f a b dx = sum (fmap area xs)
>   where xs     = takeWhile (<b) [a + 0*dx, a + 1*dx ..]
>         area x = f x * dx

For example, let's calculate the area of the right-angled triangle under $y = x$
between $x=0$ and $x=10$. As the area of a right-angled triangle is calculated as
$A = \frac{b * h}{2}$, we expect the result of \texttt{integrateApprox} to approach
$A = \frac{b * h}{2} = \frac{10 * 10}{2} = 50$ as $dx$ gets smaller

< λ integrateApprox (\textbackslash x -> x) 0 10 5
< 25
< λ integrateApprox (\textbackslash x -> x) 0 10 1
< 45
< λ integrateApprox (\textbackslash x -> x) 0 10 0.5
< 47.5
< λ integrateApprox (\textbackslash x -> x) 0 10 0.1
< 49.50000000000013
< λ integrateApprox (\textbackslash x -> x) 0 10 0.01
< 50.04999999999996

Great, it works for numeric approximations! This can be useful at times,
but not so much in our case. We want closed expressions to use when solving
physics problems, regardless of whether there are computations or not!

Luckily, the Fundamental Theorem of Calculus tells us that there IS a way to
express integrals in closed form!

From the article on [Wikipedia](https://en.wikipedia.org/wiki/Fundamental_theorem_of_calculus):

 > First part
 >
 > This part is sometimes referred to as the first fundamental theorem of calculus.
 >
 > Let $f$ be a continuous real-valued function defined on a closed
 > interval $[a, b]$. Let $F$ be the function defined, for all $x \in [a, b]$, by
 >
 > $$ F(x) = \int_a^x f(t) dt $$
 >
 > Then, $F$ is uniformly continuous on $[a,b]$, differentiable on the
 > open interval $(a, b)$ and
 >
 > $$ F'(x) = f(x) $$
 >
 > for all $x \in (a,b)$.
 >
 > Alternatively, if $f$ is merely Riemann integrable, then $F$ is
 > continuous on $[a,b]$ (but not necessarily differentiable).
 >
 > Corollary
 >
 > The fundamental theorem is often employed to compute the definite
 > integral of a function $f$ for which an antiderivative $F$ is
 > known. Specifically, if $f$ is a real-valued continuous function on
 > $[a,b]$ and $F$ is an antiderivative of $f$ in $[a,b]$ then
 >
 > $$ \int_a^b f(t) dt = F(b) - F(a) $$
 >
 > The corollary assumes continuity on the whole interval. This result is
 > strengthened slightly in the following part of the theorem.
 >
 > Second part
 >
 > This part is sometimes referred to as the second fundamental theorem
 > of calculus or the Newton–Leibniz axiom.
 >
 > Let $f$ and $f$ be real-valued functions defined on a closed interval
 > $[a,b]$ such that $f$ is continuous on all $[a,b]$ and the derivative
 > of $F$ is $f$ for almost all points in $[a,b]$. That is, $f$ and $F$
 > are functions such that for all $x \in (a,b)$ except for perhaps a set
 > of measure zero in the interval:
 >
 > $$ F'(x)=f(x) $$
 >
 > If $f$ is Riemann integrable on $[a,b]$ then
 >
 > $$ \int_a^b f(x) dx = F(b) - F(a) $$
 >
 > The second part is somewhat stronger than the corollary because it
 > does not assume that $f$ is continuous.
 >
 > When an antiderivative $f$ exists, then there are infinitely many
 > antiderivatives for $f$, obtained by adding an arbitrary constant to
 > $f$. Also, by the first part of the theorem, antiderivatives of $f$
 > always exist when $f$ is continuous.

And here's the proof. We won't delve into this, but it's quite simple.

 > Bevis
 >
 > Satsen kan bevisas enligt följande:
 >
 > \begin{align*}
 > F'(x) &= \lim_{h \to 0} \frac{F(x + h) - F(x)}{h} \\
 >       &= \lim_{h \to 0} \frac{1}{h} \left( \int_a^{x + h} f(t) dt - \int_a^x f(t) dt \right) \\
 >       &= \lim_{h \to 0} \frac{1}{h} \int_x^{x + h} f(t) dt \\
 >       &= \lim_{h \to 0} f(c) \\
 >       &= \lim_{c \to x} f(c) \\
 >       &= f(x)
 > \end{align*}
 >
 > I första steget utnyttjas derivatans definition och i det andra
 > definitionen av $f$. I det tredje steget används räknelagar för
 > integraler. I fjärde steget används medelvärdessatsen för
 > integraler. I femte steget utnyttjas det faktum att $c$ ligger mellan
 > $x$ och $x + h$, så då $h \to 0$ gäller att $c \to x$. Sista steget
 > ges av att $f$ är kontinuerlig.
 >
 > -- <cite>[Wikipedia - Analysens Fundamentalsats](https://sv.wikipedia.org/wiki/Analysens_fundamentalsats)</cite>

Let's write a function for symbolic integration of
functions. `integrate` will be the indefinite integration function, as
it's more powerful. Definite integrals can be expressed directly in
terms of indefinite integrals, but not quite vice versa.

> integrate :: Expr -> RealNum -> Expr
> integrate (f :+ g) c = (integrate f 0 + integrate g 0) + const' c
> integrate (f :- g) c = (integrate f 0 - integrate g 0) + const' c

There exists a great product rule in the case of differentiation, but
not for integration. There just doesn't exist a great way to integrate a
product that always works! The integration rule that's most analogous
to the product rule for differentiation, is integration by parts:

$$ \int f(x) g(x) dx = f(x) G(x) - \int f'(x) g(x) dx $$

Hmm, this doesn't look quite as helpful as the differentiation product rule, does it?
We want this rule to give us an expression of simpler and/or fewer integrals, and it may indeed do so.
For example, the integration of the product $x * e^x$ is a great examples of a case where it works well:

$$ \int x e^x dx = x e^x - \int 1 e^x dx = x e^x - e^x = e^x (x - 1) $$

Now THAT is a simplification.

However, just by flipping the order of the expressions, we get a case where the integration by parts rule only makes things worse:

$$ \int e^x x dx = e^x x^2 - \int e^x x dx = e^x x^2 - (e^x x^2 - \int e^x x dx) = e^x x^2 - (e^x x^2 - (e^x x^2 - \int e^x x dx)) = ... $$

Oh no, it's an infinite recursion!

There is also the problem that the integration by parts rule is simply
undefined in the case of $g(x)$ not being integrable to $G(x)$. And
so, as there exists no great way to do it, we'll settle for a mediocre
one! We'll define the integration of a product to use integration by
parts, but before integrating we'll simplify the expression in the
hopes that it will become better suited for integration.

> integrate (f :* g) c =
>     let simplified = simplify (f * g)
>     in if simplified == (f * g)
>        then f * integrate g 0 - integrate (derive f * g) 0 + const' c
>        else integrate simplified c

We get a similar rule for quotients

> integrate (f :/ g) c =
>     let simplified = simplify (f / g)
>     in if simplified == (f / g)
>        then let _F = integrate f 0
>             in _F / g + integrate (_F * (derive g / (g^2))) 0 + const' c
>        else integrate simplified c

Integration of function composition is, simply said, somewhat
complicated.  The technique to use is called "integration by
substituted", and is something like a reverse of the chain-rule of
differentiation. Luckily, most beginner-to-intermediate physics
courses purposfully avoid the use of composed functions when
integration is required, and as such, we simply won't implement it!

As long as we ensure our input functions are not composed functions,
`integrate` will still be well behaved.

> integrate (f :. g) c = error "Please don't try to integrate function compositions!"

To integrate a lambda function, we simply integrate the
body-expression with regards to the parameter variable

> integrate (Lambda p b) c = Lambda p (integrateEx b p c)

And then there are these functions. We just look up the formulas of
integration in Wolfram Alpha or something.

> integrate (Func "log") c = Lambda "x" ("x" * log "x" - "x" + const' c)
> integrate (Func "exp") c = Func "exp" + const' c
> integrate (Func "sin") c = Func "negate" :. Func "cos" + const' c
> integrate (Func "cos") c = Func "sin" + const' c
> integrate (Func "asin") c = Lambda "x" (sqrt (1 - "x"^2) + "x" * asin "x" + const' c)
> integrate (Func "acos") c = Lambda "x" ("x" * acos "x" - sqrt (1 - "x"^2) + const' c)
> integrate (Func "negate") c = Func "negate" + const' c
> integrate _ _ = undefined

> integrateEx :: Expr -> String -> RealNum -> Expr

You probably already know the rule for integrating polynomials (which
$x$ is a case of). It's just the reverse of the simple differentiation
rule!

$$ \int a_n x^n + a_{n-1} x^{n-1} + ... + a_1 x^1 + a_0 dx = \frac{a_n}{n+1} x^{n+1} + \frac{a_{n-1}}{n} x^{n} + ... + \frac{a_1}{2} x^2 + a_0 x^1 + C $$

implies that

$$ \int a dx = ax + C $$

and

$$ \int x dx = \frac{x^2}{2} + C $$

And so, we implement exactly that

> integrateEx (Const a) v c = Const a * Var v + Const c
> integrateEx (Var u) v c | u == v    = (Var u)^2 / 2 + Const c
>                         | otherwise = Var u * Var v + Const c
> integrateEx (a :+ b) v c = integrateEx a v 0 + integrateEx b v 0 + Const c
> integrateEx (a :- b) v c = integrateEx a v 0 - integrateEx b v 0 + Const c
> integrateEx (a :* b) v c =
>     let simplified = simplify (a * b)
>     in if simplified == (a * b)
>        then a * integrateEx b v 0 - integrateEx (derive a * b) v 0 + Const c
>        else integrateEx simplified v c
> integrateEx (a :/ b) v c =
>     let simplified = simplify (a / b)
>     in if simplified == (a / b)
>        then let _A = integrateEx a v 0
>             in _A / b + integrateEx (_A * (derive b / (b^2))) v 0 + Const c
>        else integrateEx simplified v c
> integrateEx e@(f :$ Const a) v c = e * Var v + Const c
> integrateEx e@(f :$ Var u) v c | v == u    = integrate f c :$ Var v
>                                  | otherwise = e * Var v + Const c
> integrateEx _ _ _ = undefined

And we're done with our DSL of calculus!

TODO: Proof/verification
----------------------------------------------------------------------

proof and/or tests go here

TODO: Examples
----------------------------------------------------------------------

they go here


















TODO: Move to after differentiation and integration and all that.
      Begin by just doing all syntactical stuff, and then end the chapter
      with evaluation, visualization, and testing.
The value of evaluation
----------------------------------------------------------------------

What comes after construction of function expressions? Well, using them of course!

One way of using a function expression is to evaluate it, and use it
just as you would a normal Haskell function. To do this, we need to
write an evaluator.

An evaluator simply takes a syntactic representation and returns the
semantic value, i.e. `eval :: SYNTAX -> SEMANTICS`.

In the case of our calculus language:

> eval :: FunExpr -> (RealNum -> RealNum)

To then evaluate a `FunExpr` is not very complicated. The elementary
functions and the `Id` function are simply substituted for their
Haskell counterparts.

> eval Exp = exp
> eval Log = log
> eval Sin = sin
> eval Cos = cos
> eval Asin = asin
> eval Acos = acos
> eval Id = id

`Const` is evaluated according to the definition $const(c) = x \mapsto c$

> eval (Const c) = \x -> c

How to evaluate arithmetic operations on functions may not be as
obvious, but we just implement them as they were defined earlier in
the chapter.

> eval (f :+ g) = \x -> (eval f x + eval g x)
> eval (f :- g) = \x -> (eval f x - eval g x)
> eval (f :* g) = \x -> (eval f x * eval g x)
> eval (f :/ g) = \x -> (eval f x / eval g x)

Function composition is similarly evaluated according to the earlier definition

> eval (f :. g) = \x -> eval f (eval g x)

TODO: these bad bois

> eval (Delta h f) = undefined
> -- eval env (Delta x) = LambdaVal "_a" (Lambda "_b" ((x' :$ "_b") - (x' :$ "_a")))
> --   where x' = subst env x
> eval (D f) = undefined
> -- eval env (D f) = eval env (simplify (derive f))
> eval (I c f) = undefined



TODO: Visualization with Hatlab
--------------------------------------------------------------------
Make us of `show` for function names, plot both function, derivative,
and integral



TODO: Verification
---------------------------------------------------------------------

QuickCheck everything. Verify that simplified expressions evaluate to
equivalent function as original




TODO: Applying our DSL to solve physics problems!
----------------------------------------------------------------------

Mostly problems regarding position, velocity, acceleration, time.

Average FOO vs. Instantaneous FOO -- Differences vs Derivatives.

Integrating to get rid of /t:s.

cool stuff here in general.

Also, many pretty pictores

> -}
