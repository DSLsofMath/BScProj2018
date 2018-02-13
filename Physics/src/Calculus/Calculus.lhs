\documentclass[12pt,a4paper]{article}

\setlength{\parindent}{0pt}
\setlength{\parskip}{0.7\baselineskip}

\usepackage{amsmath}
\usepackage{fontspec}

%%% Standard definitions from the lhs2TeX installation
%include polycode.fmt
%% Custom definitions
%include Calculus.format

\begin{document}

\title{Calculus}

\maketitle

\section{Introduction}
Calculus is cool

Differences, derivatives, and integrals

\subsection{Data type definitions and general lambda calculus stuff}

This extension will be used later to allow string literals to be implicitly
typed as Expr.

> {-# LANGUAGE OverloadedStrings #-}

Fun imports

> module Calculus.Calculus where
> import Data.Maybe
> import Data.List
> import Data.String
> import Control.Exception

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
>
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

> data Val = RealVal RealNum
>          | LambdaVal String Expr
>          | FuncVal (RealNum -> RealNum)

> valToReal (RealVal x) = x
>
> valToFunc (FuncVal f) = f
> valToFunc (LambdaVal p b) = \x -> valToReal (eval [(p, Const x)] b)

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
> eval env (D f) = eval env (simplify (deriveFn env f))

> evalBinop env a b cons op = case (eval env a, eval env b) of

Arithmetic on real numbers is just as normal

>     (RealVal a', RealVal b') -> RealVal (a' `op` b')

A nice definition for function (addition/subtraction/...) that works for
differentials: $ f + g = h $ where $ h(x) = f(x) + g(x) $

>     (LambdaVal p1 b1, LambdaVal p2 b2) ->
>         LambdaVal "_x" ((cons ((Lambda p1 b1) :$ ("_x"))
>                               ((Lambda p2 b2) :$ ("_x"))))

> evalReal :: [(String, Expr)] -> Expr -> RealNum
> evalReal env e = valToReal (eval env e)
>
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

\section{Differences}

Differences are used for stuff like average velocity.

$$ v_{avg} = \frac{x_2 - x_1}{t_2 - t_1} =\frac{\Delta x}{\Delta t} $$

This is the informal definition of the delta operator used in \textit{University Physics}:

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
We compare this to the more formal definition of \textbf{forward difference}
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

\subsection{Verification/proof/test}

???

\subsection{Examples}

> x = Lambda "t" ("t" :* (Const 5))
> id' = Lambda "x" "x"
> t = id'
> vAvg = Lambda "x" (Delta "x" :/ Delta t)
> vAvgX = vAvg :$ x
> v = eval [] (vAvgX :$ (Const 0) :$ (Const 10))

\section{Derivatives}

Derivatives are used for stuff like instantaneous velocity.

$$ v_x = \frac{dx}{dt} = lim_{\Delta t \to 0} \frac{\Delta x}{\Delta t} $$

% https://en.wikipedia.org/wiki/Leibniz%27s_notation
``In calculus, Leibniz's notation, named in honor of the 17th-century German philosopher and mathematician Gottfried Wilhelm Leibniz, uses the symbols dx and dy to represent infinitely small (or infinitesimal) increments of x and y, respectively, just as Δx and Δy represent finite increments of x and y, respectively.''

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
``Leibniz's concept of infinitesimals, long considered to be too imprecise to be used as a foundation of calculus, was eventually replaced by rigorous concepts developed by Weierstrass and others. Consequently, Leibniz's quotient notation was re-interpreted to stand for the limit of the modern definition. However, in many instances, the symbol did seem to act as an actual quotient would and its usefulness kept it popular even in the face of several competing notations. In the modern rigorous treatment of non-standard calculus, justification can be found to again consider the notation as representing an actual quotient.''

Leibniz's notation definition. Used to be defined as ``the quotient of an infinitesimal increment of y by an infinitesimal increment of x'':

$$ D(f) = \frac{dy}{dx} = \frac{lim_{\Delta y \to 0} \Delta y}{lim_{\Delta x \to 0} \Delta x} $$

% https://en.wikipedia.org/wiki/Derivative
``The most common approach to turn this intuitive idea into a precise definition is to define the derivative as a limit of difference quotients of real numbers.''

``In its modern interpretation, the expression dy/dx should not be read as the division of two quantities dx and dy (as Leibniz had envisioned it); rather, the whole expression should be seen as a single symbol that is shorthand for''

$$ D(x) = lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x} $$

which, when $ y : \mathbb{R} \to \mathbb{R} \text{ and } x \text{ is a real interval}
\leftrightarrow x \text{ is the \texttt{id} function for real numbers} $, is:

\begin{align*}
D(x) &= lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x} \\
     &= a \mapsto lim_{\Delta x \to 0} \frac{(\Delta y)(a, a + \Delta x)}{\Delta x} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + (\Delta x)(a, a + h)) - y(a)}{(\Delta x)(a, a + h)} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + ((a + h) - a)) - y(a)}{(a + h) - a} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + h) - y(a)}{h}
\end{align*}

We add the derivative syntax to the \textit{Expr} syntax tree.

<           | D Expr             -- Derivative, like "D(f)" or "f'"

Here are some derivatives. Proving these is left as an excercise to the reader:

%% TODO: Higher order functions are discrete. Typecheck to prevent differentiation
%%       of these.

> deriveFn :: [(String, Expr)] -> Expr -> Expr
> deriveFn env (f :+ g) = deriveFn env f + deriveFn env g
> deriveFn env (f :- g) = deriveFn env f - deriveFn env g
> deriveFn env (f :* g) = deriveFn env f * g + f * deriveFn env g
> deriveFn env (f :/ g) = (deriveFn env f * g - f * deriveFn env g) / (g * g)
> deriveFn env (f :. g) = Lambda "_x" ((*) (deriveFn env (g :$ "_x"))
>                                          (deriveFn env (f :$ (g :$ "_x"))))
> deriveFn env (Var v) = deriveFn env (fromJust (lookup v env))
> deriveFn env (Lambda p b) = Lambda p (deriveEx env p b)
> deriveFn env (Func "log") = Lambda "_x" (1 / "x")
> deriveFn env (Func "exp") = Func "exp"
> deriveFn env (Func "sin") = Func "cos"
> deriveFn env (Func "cos") = Func "negate" :. Func "sin"
> deriveFn env (Func "asin") = 1 / sqrt (1 - ("x" * "x"))
> deriveFn env (Func "acos") = (-1) / sqrt (1 - ("x" * "x"))
> deriveFn _ _ = undefined

> deriveEx :: [(String, Expr)] -> String -> Expr -> Expr
> deriveEx env v (Const _) = 0
> deriveEx env v (a :+ b) = deriveEx env v a + deriveEx env v b
> deriveEx env v (a :- b) = deriveEx env v a - deriveEx env v b
> deriveEx env v (a :* b) = deriveEx env v a * b + a * deriveEx env v b
> deriveEx env v (a :/ b) = (deriveEx env v a * b - a * deriveEx env v b) / (b * b)
> deriveEx env v (Var u) | u == v = 1
>                        | otherwise = 0
> deriveEx env v (f :$ e) = deriveEx env v e * (deriveFn env f :$ e)
> deriveEx _ _ _ = undefined

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

\subsection{Verification/proof/test}

???

\subsection{Examples}

> idE = Lambda "_x" "_x"
> constFn n = Lambda "_x" (Const n)

> dF = simplify . deriveFn []
> dE = simplify . deriveEx [] "x"

> test_simplify1 = (==) (simplify ("x" + "x"))
>                       (2 * "x")
> test_simplify2 = (==) (simplify (((1 + 1) * "x") + ("x" * 1)))
>                       (3 * "x")
> test_derive1   = (==) (dF (Func "sin" + idE))
>                       (Func "cos" + constFn 1)
> test_derive2   = (==) (dE (sin (sin "x")))
>                       (cos "x" * cos (sin "x"))

Let's plot graphs!

> test_plot1 = let fe = Lambda "x" ("x" * "x")
>                  fe' = dF fe
>                  f = evalF [] fe
>                  f' = evalF [] fe'
>              in plot [Fun f
>                           (show fe),
>                       Fun f'
>                           ("(D " ++ show fe ++ ") = " ++ show fe')]

\section{Integrals}

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

\end{document}
