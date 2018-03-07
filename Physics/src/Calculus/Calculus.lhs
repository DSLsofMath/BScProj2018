Introduction
======================================================================

TODO: Move relevant stuff out of the general structures/eval to their
      respective sections? (differences, derivatives, integrals)
      Or maybe, first introduce differences et al. separately, and then
      tie them together in a common section of evaluation?

TODO: Good introduction

TODO: Make the text good in general

TODO: Quotes -> good teaching text (especially in Integral secion)

TODO: Proofs/tests/verification

TODO: Improve DSLs a bit? I don't like the `Expr` tree very much.
      Separate into `FunExpr` and `Expr`, where `Expr` is very simple?

TODO: Have someone critique this

Calculus is cool

Differences, derivatives, and integrals

Data type definitions and general lambda calculus stuff
----------------------------------------------------------------------

This extension will be used later to allow string literals to be implicitly
typed as Expr.

> {-# LANGUAGE OverloadedStrings #-}

Fun imports

> module Calculus.Calculus where
> import Data.Maybe
> import Data.List
> import Data.String
> import Control.Exception
> import Data.Tree.Pretty

Simple graph plotting library

> import Hatlab.Plot

A real number. Double is mostly an adequate representation

> type RealNum = Double

The syntax tree of an expression

> data Expr = Const RealNum      -- Real constant
>           | Expr :+ Expr       -- Plus (Addition)
>           | Expr :- Expr       -- Minus (Subtraction)
>           | Expr :* Expr       -- Times (Multiplication)
>           | Expr :/ Expr       -- Divided by (Division)
>           | Expr :. Expr       -- Composition (After, o)
>           | Var String         -- Variable
>           | Func String        -- Builtin function
>           | Lambda String Expr -- Lambda function
>           | Delta Expr         -- Difference, like "Δx"
>           | D Expr             -- Derivative, like "f'"
>           | Expr :$ Expr       -- Function application
>   deriving Eq

> data Tree e = Leaf e 
>             | Plus (Tree e) (Tree e)
>             | Minus (Tree e) (Tree e)

> makeTree :: Expr -> Tree Expr
> makeTree (Const x)  = Leaf (Const x)
> makeTree (e1 :+ e2) = Plus (makeTree e1) (makeTree e2)

> pp :: Tree Expr -> String
> pp (Leaf c) = show ic

> e1 = (Const 2 :+ Const 3) :* (Const 3 :+ Const 2)
> e2 = Const (25)

> equals :: Expr -> Expr -> Bool
> -- Addition is commutative
> equals (e1 :+ e2) (e3 :+ e4) = (canonify (e1 :+ e2) == canonify (e3 :+ e4)) || 
>                                (canonify (e1 :+ e2) == canonify (e4 :+ e3))
> -- | Addition is associative
> -- equals (e1 :+ (e2 :+ e3))    = undefined
> -- Multiplication is commutative
> equals (e1 :* e2) (e3 :* e4) = (canonify (e1 :* e2) == canonify (e3 :* e4)) || 
>                                (canonify (e1 :* e2) == canonify (e4 :* e3))
> equals e1 e2 = canonify e1 == canonify e2

> canonify :: Expr -> Expr
> -- | Addition
> -- | e + 0 = e
> canonify (e :+ (Const 0)) = canonify e
> canonify ((Const 0) :+ e) = canonify e
> -- | Lifting
> canonify (Const x :+ Const y) = Const (x + y)
> canonify (e1 :+ e2) = canonify $ canonify e1 :+ canonify e2

> -- | Subtraction
> -- | e - 0 = e
> canonify (e :- Const 0) = canonify e
> -- | 0 - b = -b
> canonify (Const 0 :- b) = canonify (negate (canonify b))
> -- | Lifting
> canonify (Const a :- Const b) = Const (a - b)
> canonify (e1 :- e2) = canonify $ canonify e1 :- canonify e2

> -- | Multiplication
> -- | e * 0 = 0
> canonify (_ :* Const 0) = Const 0
> canonify (Const 0 :* _) = Const 0
> -- | e * 1 = e
> canonify (e :* (Const 1)) = canonify e
> canonify ((Const 1) :* e) = canonify e
> -- | Lifting
> canonify (Const a :* Const b) = Const (a * b)
> -- | Propagate
> canonify (e1 :* e2) = canonify $ canonify e1 :* canonify e2

> -- | Division
> canonify (Const a :/ Const b) = Const (a / b)
> canonify (e1 :/ e2) = canonify $ canonify e1 :/ canonify e2

> -- | Lambda
> canonify (Lambda p b) = (Lambda p (canonify b))

> -- | Catch all
> canonify e = e



A `const` and `id` function could be useful. We can describe them like this:

> const' c = Lambda "x" (Const c)
>
> id' = Lambda "x" "x"

We implement Num, Fractal, Floating, and IsString for Expr to make it nicer to use

> instance Num Expr where
>       a + b = a :+ b
>       a - b = a :- b
>       a * b = a :* b
>       abs e = Func "abs" :$ e
>       signum e = Func "signum" :$ e
>       fromInteger = Const . fromInteger

> instance Fractional Expr where
>       a / b = a :/ b
>       fromRational = Const . fromRational

> instance Floating Expr where
>     pi = Const pi
>     exp e = Func "exp" :$ e
>     log e = Func "log" :$ e
>     sin e = Func "sin" :$ e
>     cos e = Func "cos" :$ e
>     asin e = Func "asin" :$ e
>     acos e = Func "acos" :$ e
>     atan e = Func "atan" :$ e
>     sinh = undefined; cosh = undefined; asinh = undefined; acosh = undefined; atanh = undefined;

> instance IsString Expr where
>     fromString = Var

We want to be able to print our expressions in a human-readable format

> instance Show Expr where
>     show (Const x) = show x
>     show (a :+ b) = "(" ++ show a ++ " + " ++ show b ++ ")"
>     show (a :- b) = "(" ++ show a ++ " - " ++ show b ++ ")"
>     show (a :* b) = "(" ++ show a ++ " * " ++ show b ++ ")"
>     show (a :/ b) = "(" ++ show a ++ " / " ++ show b ++ ")"
>     show (f :. g) = "(" ++ show f ++ " ∘ " ++ show g ++ ")"
>     show (Var v) = v
>     show (Func f) = f
>     show (Lambda p b) = "(lamda " ++ p ++ " . " ++ show b ++ ")"
>     show (Delta x) = "(delta " ++ show x ++ ")"
>     show (D e) = "(D " ++ show e ++ ")"
>     show (f :$ e) = "(" ++ show f ++ " " ++ show e ++ ")"

> avg (y1, x1) (y2, x2) = (y2 - y1) / (x2 - x1)

is equivalent to

> avg' y x t1 t2 = ((y :$ t2) - (y :$ t1)) / ((x :$ t2) - (x :$ t1))

which is equivalent to

> avg'' y x = Delta y / Delta x

`eval` evaluates an expression. Converts from syntactic domain to semantic domain.

> eval :: [(String, Expr)] -> Expr -> Val
> eval env (Const x) = RealVal x
> eval env (a :+ b) = evalBinop env a b (:+) (+)
> eval env (a :- b) = evalBinop env a b (:-) (-)
> eval env (a :* b) = evalBinop env a b (:*) (*)
> eval env (a :/ b) = evalBinop env a b (:/) (/)
> eval env (f :. g) = eval env (Lambda "_x" (f :$ (g :$ ("_x"))))
> eval env (Var s) =
>     eval env (fromMaybe (error ("Variable "++s++" is not in environment: "++show env))
>                         (lookup s env))
> eval env (Lambda p b) = LambdaVal p (subst env b)
> eval env (Func "negate") = FuncVal negate
> eval env (Func "abs") = FuncVal abs
> eval env (Func "signum") = FuncVal signum
> eval env (Func "log") = FuncVal log
> eval env (Func "exp") = FuncVal exp
> eval env (Func "cos") = FuncVal cos
> eval env (Func "sin") = FuncVal sin
> eval env (Func "asin") = FuncVal asin
> eval env (Func "acos") = FuncVal acos
> eval env (Func "atan") = FuncVal atan
> eval env (f :$ arg) = case (eval env f) of
>     LambdaVal p b -> eval [(p, subst env arg)] b
>     FuncVal f     -> RealVal (f (valToReal (eval env arg)))
>     _             -> error "Not a function"
> eval env (Delta x) = LambdaVal "_a" (Lambda "_b" ((x' :$ ("_b")) - (x' :$ ("_a"))))
>   where x' = subst env x
> eval env (D f) = eval env (simplify (derive f))

> evalBinop env a b cons op = case (eval env a, eval env b) of

Arithmetic on real numbers is just as normal

>     (RealVal a', RealVal b') -> RealVal (a' `op` b')

A nice definition for function (addition/subtraction/...) that works for
differentials: $f + g = h$ where $h(x) = f(x) + g(x)$

>     (LambdaVal p1 b1, LambdaVal p2 b2) ->
>         LambdaVal "_x" ((cons ((Lambda p1 b1) :$ ("_x"))
>                               ((Lambda p2 b2) :$ ("_x"))))

The semantic value of an evaluation. Can either be a real number, a haskell function, or a lambda(?)
TODO: Should a lambda really be returnable here? Kinda makes sense, kinda doesn't...

> data Val = RealVal RealNum
>          | LambdaVal String Expr
>          | FuncVal (RealNum -> RealNum)

> instance Show Val where
>   show (RealVal n) = show n

Helper functions to improve ergonomics of evaluation

> valToReal (RealVal x) = x

> valToFunc (FuncVal f) = f
> valToFunc (LambdaVal p b) = \x -> valToReal (eval [(p, Const x)] b)

> evalReal :: [(String, Expr)] -> Expr -> RealNum
> evalReal env e = valToReal (eval env e)

> evalF :: [(String, Expr)] -> Expr -> (RealNum -> RealNum)
> evalF env e = valToFunc (eval env e)

Substitution function to instantiate expression for environment

> subst :: [(String, Expr)] -> Expr -> Expr
> subst env (a :+ b) = subst env a :+ subst env b
> subst env (a :- b) = subst env a :- subst env b
> subst env (a :* b) = subst env a :* subst env b
> subst env (a :/ b) = subst env a :/ subst env b
> subst env (a :. b) = subst env a :. subst env b
> subst env (Var s) = case (lookup s env) of
>     Just e  -> e
>     Nothing -> (Var s)
> subst env (Lambda p b) = Lambda p (subst env b)
> subst env (f :$ arg) = subst env f :$ subst env arg
> subst env (Delta x) = Delta (subst env x)
> subst env (D f) = D (subst env f)
> subst _ e = e

Differences
----------------------------------------------------------------------

Differences are used for stuff like average velocity.

$$ v_{avg} = \frac{x_2 - x_1}{t_2 - t_1} = \frac{\Delta x}{\Delta t} $$

This is the informal definition of the delta operator used in *University Physics*:

$$ \Delta x = x_2 - x_1 $$

Ok, so it's a difference. But what does $x_2$ and $x_1$ mean, and what do they come from?
$x_2$ and $x_1$ are not explicitly bound anywhere,
but seems reasonable to assume that $x_i \in x$ or equivalently, that $x$
is a function with a subscript index as an argument, that returns a $\mathbb{R}$.

Further, the indices $1,2$ should not be thought of as specific constants,
but rather arbitrary real number variables identified by these integers.
Lets call them $a,b$ instead, to make it clear that they are not constants.

$$ \Delta x = x_b - x_a $$

Now $a,b$ are implicitly bound. We make the binding explicit.

$$ (\Delta x)(a, b) = x_b - x_a $$

% https://en.wikipedia.org/wiki/Finite_difference

We compare this to the more formal definition of **forward difference**
from wikipedia:

$$ \Delta_h[f](x) = f(x + h) - f(x) $$

The parameter bindings are a bit all over the place here. To move easily compare
to our definition, let's rename $x$ to $a$ and $f$ to $x$, and change the parameter
declaration syntax:

$$ (\Delta x)(h)(a) = x(a + h) - x(a) $$

This is almost identical to the definition we arrived at earlier, with the
exception of expressing $b$ as $a + h$. We'll use our own definition hereinafter.

We express our definition of $\Delta$ in Haskell:

< delta :: (RealNum -> RealNum) -> RealNum -> RealNum -> RealNum
< delta x a b = x(b) - x(a)

or

< delta x = \a -> \b -> x(b) - x(a)

This is a shallow embedding. Let's look at how it's expressed in our
deep embedding:

This is the representation of the delta operator in the syntax tree.
The argument will need to be type-checked to ensure that it's a function.

<           | Delta Expr         -- Difference, like "Δx"

We implement the delta case of the eval function according to the definition

< eval env (Delta x) =
<     LambdaVal env
<               "_a"
<               (Lambda "_b"
<                       ((x :$ ("_b")) :- (x :$ ("_a"))))

Verification/proof/test
----------------------------------------------------------------------

???

Examples
----------------------------------------------------------------------

> x = Lambda "t" ("t" :* (Const 5))
> t = id'
> vAvg = Lambda "x" (Delta "x" :/ Delta t)
> vAvgX = vAvg :$ x
> v = eval [] (vAvgX :$ (Const 0) :$ (Const 10))

Derivatives
======================================================================

Derivatives are used for stuff like instantaneous velocity.

$$ v_x = \frac{dx}{dt} = lim_{\Delta t \to 0} \frac{\Delta x}{\Delta t} $$

% https://en.wikipedia.org/wiki/Leibniz%27s_notation

 > In calculus, Leibniz's notation, named in honor of the 17th-century
 > German philosopher and mathematician Gottfried Wilhelm Leibniz,
 > uses the symbols dx and dy to represent infinitely small (or
 > infinitesimal) increments of x and y, respectively, just as Δx and
 > Δy represent finite increments of x and y, respectively.

We interpret this in mathematical terms:

$$ df = lim_{\Delta f \to 0} \Delta f $$

such that

$$ \forall y(x), D(y) = \frac{dy}{dx} = \frac{lim_{\Delta y \to 0} \Delta y}
                                             {lim_{\Delta x \to 0} \Delta x} $$

This definition of derivatives is very appealing, as it suggests a very
simple and intuitive transition from finite differences to infinitesimal
differentials.

This concept of infinitesimals is very intuitive, and the ability to manipulate
differentials algebraically is very useful. However, this concept is generally
considered too imprecise to be used as the foundation of calculus.

% https://en.wikipedia.org/wiki/Leibniz%27s_notation

 > Leibniz's concept of infinitesimals, long considered to be too
 > imprecise to be used as a foundation of calculus, was eventually
 > replaced by rigorous concepts developed by Weierstrass and
 > others. Consequently, Leibniz's quotient notation was re-interpreted
 > to stand for the limit of the modern definition. However, in many
 > instances, the symbol did seem to act as an actual quotient would and
 > its usefulness kept it popular even in the face of several competing
 > notations. In the modern rigorous treatment of non-standard calculus,
 > justification can be found to again consider the notation as
 > representing an actual quotient.

Leibniz's notation definition. Used to be defined as "the quotient of an infinitesimal increment of y by an infinitesimal increment of x":

$$ D(f) = \frac{dy}{dx} = \frac{lim_{\Delta y \to 0} \Delta y}{lim_{\Delta x \to 0} \Delta x} $$

% https://en.wikipedia.org/wiki/Derivative

 > The most common approach to turn this intuitive idea into a
 > precise definition is to define the derivative as a limit of
 > difference quotients of real numbers.

 > In its modern interpretation, the expression dy/dx should not be read
 > as the division of two quantities dx and dy (as Leibniz had envisioned
 > it); rather, the whole expression should be seen as a single symbol
 > that is shorthand for

$$ D(x) = lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x} $$

which, when $y : \mathbb{R} \to \mathbb{R}$ and $x$ is a real interval
$\leftrightarrow x$ is the $id$ function for real numbers, is:

\begin{align*}
D(x) &= lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x} \\
     &= a \mapsto lim_{\Delta x \to 0} \frac{(\Delta y)(a, a + \Delta x)}{\Delta x} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + (\Delta x)(a, a + h)) - y(a)}{(\Delta x)(a, a + h)} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + ((a + h) - a)) - y(a)}{(a + h) - a} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + h) - y(a)}{h}
\end{align*}

We add the derivative syntax to the *Expr* syntax tree.

<              | D Expr             -- Derivative, like "D(f)" or "f'"

Here are some derivatives. Proving these is left as an excercise to the reader:

% TODO: Higher order functions are discrete. Typecheck to prevent differentiation
%       of these.

> derive :: Expr -> Expr
> derive (f :+ g) = derive f + derive g
> derive (f :- g) = derive f - derive g
> derive (f :* g) = derive f * g + f * derive g
> derive (f :/ g) = (derive f * g - f * derive g) / (g^2)
> derive (f :. g) = derive g * (derive f :. g)
> derive (Lambda p b) = Lambda p (deriveEx b p)
> derive (Func "log") = Lambda "x" (1 / "x")
> derive (Func "exp") = Func "exp"
> derive (Func "sin") = Func "cos"
> derive (Func "cos") = Func "negate" :. Func "sin"
> derive (Func "asin") = Lambda "x" (1 / sqrt (1 - ("x" * "x")))
> derive (Func "acos") = Lambda "x" ((-1) / sqrt (1 - ("x" * "x")))
> derive (Func "negate") = Func "negate"
> derive _ = undefined

> deriveEx :: Expr -> String -> Expr
> deriveEx (Const _) v = 0
> deriveEx (a :+ b) v = deriveEx a v + deriveEx b v
> deriveEx (a :- b) v = deriveEx a v - deriveEx b v
> deriveEx (a :* b) v = deriveEx a v * b + a * deriveEx b v
> deriveEx (a :/ b) v = (deriveEx a v * b - a * deriveEx b v) / b^2
> deriveEx (Var u) v | u == v = 1
>                    | otherwise = 0
> deriveEx (f :$ e) v = deriveEx e v * (derive f :$ e)
> deriveEx _ _ = undefined

Difficult to read some of these derivatives. Let's simplify

> simplify :: Expr -> Expr
> simplify (Const 0 :* b) = 0
> simplify (Const 1 :* b) = simplify b
> simplify (Const a :* Const b) = Const (a * b)
> simplify ((Const a :* b) :+ c) | b' == c'  = Const (a + 1) :* b'
>                                | otherwise = (Const a :* b') :+ c'
>   where b' = simplify b
>         c' = simplify c
> simplify (c :+ (Const a :* b)) = simplify ((Const a :* b) :+ c)
> simplify (Const a :* b) = Const a :* simplify b
> simplify (a :* Const b) = simplify (Const b :* a)
> simplify (a :* b) = simplify a :* simplify b
> simplify (Const 0 :+ b) = simplify b
> simplify (Const a :+ Const b) = Const (a + b)
> simplify (Const a :+ b) = Const a :+ simplify b
> simplify (a :+ Const b) = simplify (Const b :+ a)
> simplify (a :+ b) | a' == b'             = simplify (2 * a')
>                   | (a + b) == (a' + b') = a + b
>                   | otherwise            = simplify (a' + b')
>   where a' = simplify a
>         b' = simplify b
> simplify (Const 0 :- b) = simplify (negate (simplify b))
> simplify (Const a :- Const b) = Const (a - b)
> simplify (Const a :- b) = Const a :- simplify b
> simplify (a :- Const b) = simplify (Const (0-b) :+ a)
> simplify (Lambda p b) = (Lambda p (simplify b))
> simplify e = e

Verification/proof/test
----------------------------------------------------------------------

???

Examples
----------------------------------------------------------------------

> idE = Lambda "_x" "_x"

> dF = simplify . derive
> dE = simplify . (flip deriveEx) "x"

> test_simplify1 = (==) (simplify ("x" + "x"))
>                       (2 * "x")
> test_simplify2 = (==) (simplify (((1 + 1) * "x") + ("x" * 1)))
>                       (3 * "x")
> test_derive1   = (==) (dF (Func "sin" + idE))
>                       (Func "cos" + const' 1)
> test_derive2   = (==) (dE (sin (sin (Var "x"))))
>                       (cos "x" * cos (sin (Var "x")))

Let's plot graphs!

> test_plot1 = let fe = Lambda "x" ("x" * "x")
>                  fe' = dF fe
>                  f = evalF [] fe
>                  f' = evalF [] fe'
>              in plot [Fun f
>                           (show fe),
>                       Fun f'
>                           ("(D " ++ show fe ++ ") = " ++ show fe')]

Integrals
======================================================================

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

TODO: Describe relationship between between "Definite integrals", "Indefinite integrals", and "Antiderivatives".

The following quotes are cut from [Wikipedia](https://en.wikipedia.org/wiki/Fundamental_theorem_of_calculus).

 > The fundamental theorem of calculus is a theorem that links the
 > concept of differentiating a function with the concept of integrating
 > a function.
 >
 > The first part of the theorem, sometimes called the first fundamental
 > theorem of calculus, states that one of the antiderivatives (also
 > called indefinite integral), say F, of some function f may be obtained
 > as the integral of f with a variable bound of integration. This
 > implies the existence of antiderivatives for continuous functions.[1]
 >
 > Conversely, the second part of the theorem, sometimes called the
 > second fundamental theorem of calculus, states that the integral of a
 > function f over some interval can be computed by using any one, say F,
 > of its infinitely many antiderivatives. This part of the theorem has
 > key practical applications, because explicitly finding the
 > antiderivative of a function by symbolic integration allows for
 > avoiding numerical integration to compute integrals.

 > Newton and Leibniz
 >
 > The major advance in integration came in the 17th century with the
 > independent discovery of the fundamental theorem of calculus by Newton
 > and Leibniz. The theorem demonstrates a connection between integration
 > and differentiation. This connection, combined with the comparative
 > ease of differentiation, can be exploited to calculate integrals. In
 > particular, the fundamental theorem of calculus allows one to solve a
 > much broader class of problems. Equal in importance is the
 > comprehensive mathematical framework that both Newton and Leibniz
 > developed. Given the name infinitesimal calculus, it allowed for
 > precise analysis of functions within continuous domains. This
 > framework eventually became modern calculus, whose notation for
 > integrals is drawn directly from the work of Leibniz.  Formalization
 > While Newton and Leibniz provided a systematic approach to
 > integration, their work lacked a degree of rigour. Bishop Berkeley
 > memorably attacked the vanishing increments used by Newton, calling
 > them "ghosts of departed quantities". Calculus acquired a firmer
 > footing with the development of limits. Integration was first
 > rigorously formalized, using limits, by Riemann. Although all bounded
 > piecewise continuous functions are Riemann-integrable on a bounded
 > interval, subsequently more general functions were
 > considered—particularly in the context of Fourier analysis—to which
 > Riemann's definition does not apply, and Lebesgue formulated a
 > different definition of integral, founded in measure theory (a
 > subfield of real analysis). Other definitions of integral, extending
 > Riemann's and Lebesgue's approaches, were proposed. These approaches
 > based on the real number system are the ones most common today, but
 > alternative approaches exist, such as a definition of integral as the
 > standard part of an infinite Riemann sum, based on the hyperreal
 > number system.


Newton and Leibniz independently discovered the fundemental theorem of calculus.
They based their definitions on infinitesimals which, as described above was considered too imprecise.
Later, Riemann rigorously formalized integration using limits.

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

![Riemann sum convergence, (C) KSmrq](https://upload.wikimedia.org/wikipedia/commons/2/2a/Riemann_sum_convergence.png)

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
> integrateEx e@(f :$ (Const a)) v c = e * Var v + Const c
> integrateEx e@(f :$ (Var u)) v c | v == u    = integrate f c :$ Var v
>                                  | otherwise = e * Var v + Const c
> integrateEx _ _ _ = undefined

And we're done with our DSL of calculus!

TODO: Proof/verification
----------------------------------------------------------------------

proof and/or tests go here

TODO: Examples
----------------------------------------------------------------------

they go here

TODO: Applying our DSL to solve physics problems!
----------------------------------------------------------------------

Mostly problems regarding position, velocity, acceleration, time.

Average FOO vs. Instantaneous FOO -- Differences vs Derivatives.

Integrating to get rid of /t:s.

cool stuff here in general.

Also, many pretty pictores
