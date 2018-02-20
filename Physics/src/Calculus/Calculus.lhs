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

%% TODO: Move relevant stuff out of the general structures/eval to their
%%       respective sections? (differences, derivatives, integrals)
%%       Or maybe, first introduce differences et al. separately, and then
%%       tie them together in a common section of evaluation?

Calculus is cool

Differences, derivatives, and integrals

\subsection{Data type definitions and general lambda calculus stuff}

This extension will be used later to allow string literals to be implicitly
typed as Expr.

> {-# LANGUAGE OverloadedStrings #-}

Fun imports

\begin{code}
module Calculus.Calculus where
import Data.Maybe
import Data.List
import Data.String
import Control.Exception
\end{code}

Simple graph plotting library

\begin{code}
import Hatlab.Plot
\end{code}

A real number. Double is mostly an adequate representation

\begin{code}
type RealNum = Double
\end{code}

The syntax tree of an expression

\begin{code}
data Expr = Const RealNum      -- Real constant
          | Expr :+ Expr       -- Plus (Addition)
          | Expr :- Expr       -- Minus (Subtraction)
          | Expr :* Expr       -- Times (Multiplication)
          | Expr :/ Expr       -- Divided by (Division)
          | Expr :. Expr       -- Composition (After, o)
          | Var String         -- Variable
          | Func String        -- Builtin function
          | Lambda String Expr -- Lambda function
          | Delta Expr         -- Difference, like "Δx"
          | D Expr             -- Derivative, like "f'"
          | Expr :$ Expr       -- Function application
  deriving Eq
\end{code}

We implement Num, Fractal, Floating, and IsString for Expr to make it nicer to use

\begin{code}
instance Num Expr where
      a + b = a :+ b
      a - b = a :- b
      a * b = a :* b
      abs e = Func "abs" :$ e
      signum e = Func "signum" :$ e
      fromInteger = Const . fromInteger

instance Fractional Expr where
      a / b = a :/ b
      fromRational = Const . fromRational

instance Floating Expr where
    pi = Const pi
    exp e = Func "exp" :$ e
    log e = Func "log" :$ e
    sin e = Func "sin" :$ e
    cos e = Func "cos" :$ e
    asin e = Func "asin" :$ e
    acos e = Func "acos" :$ e
    atan e = Func "atan" :$ e
    sinh = undefined; cosh = undefined; asinh = undefined; acosh = undefined; atanh = undefined;

instance IsString Expr where
    fromString = Var
\end{code}

We want to be able to print our expressions in a human-readable format

\begin{code}
instance Show Expr where
    show (Const x) = show x
    show (a :+ b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (a :- b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (a :* b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (a :/ b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (f :. g) = "(" ++ show f ++ " ∘ " ++ show g ++ ")"
    show (Var v) = v
    show (Func f) = f
    show (Lambda p b) = "(lamda " ++ p ++ " . " ++ show b ++ ")"
    show (Delta x) = "(delta " ++ show x ++ ")"
    show (D e) = "(D " ++ show e ++ ")"
    show (f :$ e) = "(" ++ show f ++ " " ++ show e ++ ")"
\end{code}

\begin{code}
avg (y1, x1) (y2, x2) = (y2 - y1) / (x2 - x1)
\end{code}

is equivalent to

\begin{code}
avg' y x t1 t2 = ((y :$ t2) - (y :$ t1)) / ((x :$ t2) - (x :$ t1))
\end{code}

which is equivalent to

\begin{code}
avg'' y x = Delta y / Delta x
\end{code}

\texttt{eval} evaluates an expression. Converts from syntactic domain to semantic domain.

\begin{code}
eval :: [(String, Expr)] -> Expr -> Val
eval env (Const x) = RealVal x
eval env (a :+ b) = evalBinop env a b (:+) (+)
eval env (a :- b) = evalBinop env a b (:-) (-)
eval env (a :* b) = evalBinop env a b (:*) (*)
eval env (a :/ b) = evalBinop env a b (:/) (/)
eval env (f :. g) = eval env (Lambda "_x" (f :$ (g :$ ("_x"))))
eval env (Var s) =
    eval env (fromMaybe (error ("Variable "++s++" is not in environment: "++show env))
                        (lookup s env))
eval env (Lambda p b) = LambdaVal p (subst env b)
eval env (Func "negate") = FuncVal negate
eval env (Func "abs") = FuncVal abs
eval env (Func "signum") = FuncVal signum
eval env (Func "log") = FuncVal log
eval env (Func "exp") = FuncVal exp
eval env (Func "cos") = FuncVal cos
eval env (Func "sin") = FuncVal sin
eval env (Func "asin") = FuncVal asin
eval env (Func "acos") = FuncVal acos
eval env (Func "atan") = FuncVal atan
eval env (f :$ arg) = case (eval env f) of
    LambdaVal p b -> eval [(p, subst env arg)] b
    FuncVal f     -> RealVal (f (valToReal (eval env arg)))
    _             -> error "Not a function"
eval env (Delta x) = LambdaVal "_a" (Lambda "_b" ((x' :$ ("_b")) - (x' :$ ("_a"))))
  where x' = subst env x
eval env (D f) = eval env (simplify (deriveFn env f))

evalBinop env a b cons op = case (eval env a, eval env b) of
\end{code}

Arithmetic on real numbers is just as normal

\begin{code}
    (RealVal a', RealVal b') -> RealVal (a' `op` b')
\end{code}

A nice definition for function (addition/subtraction/...) that works for
differentials: $ f + g = h $ where $ h(x) = f(x) + g(x) $

\begin{code}
    (LambdaVal p1 b1, LambdaVal p2 b2) ->
        LambdaVal "_x" ((cons ((Lambda p1 b1) :$ ("_x"))
                              ((Lambda p2 b2) :$ ("_x"))))
\end{code}

The semantic value of an evaluation. Can either be a real number, a haskell function, or a lambda(?)
TODO: Should a lambda really be returnable here? Kinda makes sense, kinda doesn't...

\begin{code}
data Val = RealVal RealNum
         | LambdaVal String Expr
         | FuncVal (RealNum -> RealNum)
\end{code}

Helper functions to improve ergonomics of evaluation

\begin{code}
valToReal (RealVal x) = x

valToFunc (FuncVal f) = f
valToFunc (LambdaVal p b) = \x -> valToReal (eval [(p, Const x)] b)

evalReal :: [(String, Expr)] -> Expr -> RealNum
evalReal env e = valToReal (eval env e)

evalF :: [(String, Expr)] -> Expr -> (RealNum -> RealNum)
evalF env e = valToFunc (eval env e)
\end{code}

Substitution function to instantiate expression for environment

\begin{code}
subst :: [(String, Expr)] -> Expr -> Expr
subst env (a :+ b) = subst env a :+ subst env b
subst env (a :- b) = subst env a :- subst env b
subst env (a :* b) = subst env a :* subst env b
subst env (a :/ b) = subst env a :/ subst env b
subst env (a :. b) = subst env a :. subst env b
subst env (Var s) = case (lookup s env) of
    Just e  -> e
    Nothing -> (Var s)
subst env (Lambda p b) = Lambda p (subst env b)
subst env (f :$ arg) = subst env f :$ subst env arg
subst env (Delta x) = Delta (subst env x)
subst env (D f) = D (subst env f)
subst _ e = e
\end{code}

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

\begin{spec}
delta :: (RealNum -> RealNum) -> RealNum -> RealNum -> RealNum
delta x a b = x(b) - x(a)
\end{spec}

or

\begin{spec}
delta x = \a -> \b -> x(b) - x(a)
\end{spec}

This is a shallow embedding. Let's look at how it's expressed in our
deep embedding:

This is the representation of the delta operator in the syntax tree.
The argument will need to be type-checked to ensure that it's a function.

\begin{spec}
          | Delta Expr         -- Difference, like "Δx"
\end{spec}

We implement the delta case of the eval function according to the definition

\begin{spec}
eval env (Delta x) =
    LambdaVal env
              "_a"
              (Lambda "_b"
                      ((x :$ ("_b")) :- (x :$ ("_a"))))
\end{spec}

\subsection{Verification/proof/test}

???

\subsection{Examples}

\begin{code}
x = Lambda "t" ("t" :* (Const 5))
id' = Lambda "x" "x"
t = id'
vAvg = Lambda "x" (Delta "x" :/ Delta t)
vAvgX = vAvg :$ x
v = eval [] (vAvgX :$ (Const 0) :$ (Const 10))
\end{code}

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
\leftrightarrow x \text{ is the } id \text{function for real numbers} $, is:

\begin{align*}
D(x) &= lim_{\Delta x \to 0} \frac{\Delta y}{\Delta x} \\
     &= a \mapsto lim_{\Delta x \to 0} \frac{(\Delta y)(a, a + \Delta x)}{\Delta x} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + (\Delta x)(a, a + h)) - y(a)}{(\Delta x)(a, a + h)} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + ((a + h) - a)) - y(a)}{(a + h) - a} \\
     &= a \mapsto lim_{h \to 0} \frac{y(a + h) - y(a)}{h}
\end{align*}

We add the derivative syntax to the \textit{Expr} syntax tree.

\begin{spec}
             | D Expr             -- Derivative, like "D(f)" or "f'"
\end{spec}

Here are some derivatives. Proving these is left as an excercise to the reader:

%% TODO: Higher order functions are discrete. Typecheck to prevent differentiation
%%       of these.

\begin{code}
deriveFn :: [(String, Expr)] -> Expr -> Expr
deriveFn env (f :+ g) = deriveFn env f + deriveFn env g
deriveFn env (f :- g) = deriveFn env f - deriveFn env g
deriveFn env (f :* g) = deriveFn env f * g + f * deriveFn env g
deriveFn env (f :/ g) = (deriveFn env f * g - f * deriveFn env g) / (g * g)
deriveFn env (f :. g) = Lambda "_x" ((*) (deriveFn env (g :$ "_x"))
                                         (deriveFn env (f :$ (g :$ "_x"))))
deriveFn env (Var v) = deriveFn env (fromJust (lookup v env))
deriveFn env (Lambda p b) = Lambda p (deriveEx env p b)
deriveFn env (Func "log") = Lambda "_x" (1 / "x")
deriveFn env (Func "exp") = Func "exp"
deriveFn env (Func "sin") = Func "cos"
deriveFn env (Func "cos") = Func "negate" :. Func "sin"
deriveFn env (Func "asin") = 1 / sqrt (1 - ("x" * "x"))
deriveFn env (Func "acos") = (-1) / sqrt (1 - ("x" * "x"))
deriveFn _ _ = undefined

deriveEx :: [(String, Expr)] -> String -> Expr -> Expr
deriveEx env v (Const _) = 0
deriveEx env v (a :+ b) = deriveEx env v a + deriveEx env v b
deriveEx env v (a :- b) = deriveEx env v a - deriveEx env v b
deriveEx env v (a :* b) = deriveEx env v a * b + a * deriveEx env v b
deriveEx env v (a :/ b) = (deriveEx env v a * b - a * deriveEx env v b) / (b * b)
deriveEx env v (Var u) | u == v = 1
                       | otherwise = 0
deriveEx env v (f :$ e) = deriveEx env v e * (deriveFn env f :$ e)
deriveEx _ _ _ = undefined
\end{code}

Difficult to read some of these derivatives. Let's simplify

\begin{code}
simplify :: Expr -> Expr
simplify (Const 0 :* b) = 0
simplify (Const 1 :* b) = simplify b
simplify (Const a :* Const b) = Const (a * b)
simplify ((Const a :* b) :+ c) | b' == c'  = Const (a + 1) :* b'
                               | otherwise = (Const a :* b') :+ c'
  where b' = simplify b
        c' = simplify c
simplify (c :+ (Const a :* b)) = simplify ((Const a :* b) :+ c)
simplify (Const a :* b) = Const a :* simplify b
simplify (a :* Const b) = simplify (Const b :* a)
simplify (a :* b) = simplify a :* simplify b
simplify (Const 0 :+ b) = simplify b
simplify (Const a :+ Const b) = Const (a + b)
simplify (Const a :+ b) = Const a :+ simplify b
simplify (a :+ Const b) = simplify (Const b :+ a)
simplify (a :+ b) | a' == b'             = simplify (2 * a')
                  | (a + b) == (a' + b') = a + b
                  | otherwise            = simplify (a' + b')
  where a' = simplify a
        b' = simplify b
simplify (Const 0 :- b) = simplify (negate (simplify b))
simplify (Const a :- Const b) = Const (a - b)
simplify (Const a :- b) = Const a :- simplify b
simplify (a :- Const b) = simplify (Const (0-b) :+ a)
simplify (Lambda p b) = (Lambda p (simplify b))
simplify e = e
\end{code}

\subsection{Verification/proof/test}

???

\subsection{Examples}

\begin{code}
idE = Lambda "_x" "_x"
constFn n = Lambda "_x" (Const n)

dF = simplify . deriveFn []
dE = simplify . deriveEx [] "x"

test_simplify1 = (==) (simplify ("x" + "x"))
                      (2 * "x")
test_simplify2 = (==) (simplify (((1 + 1) * "x") + ("x" * 1)))
                      (3 * "x")
test_derive1   = (==) (dF (Func "sin" + idE))
                      (Func "cos" + constFn 1)
test_derive2   = (==) (dE (sin (sin "x")))
                      (cos "x" * cos (sin "x"))
\end{code}

Let's plot graphs!

\begin{code}
test_plot1 = let fe = Lambda "x" ("x" * "x")
                 fe' = dF fe
                 f = evalF [] fe
                 f' = evalF [] fe'
             in plot [Fun f
                          (show fe),
                      Fun f'
                          ("(D " ++ show fe ++ ") = " ++ show fe')]
\end{code}

\section{Integrals}

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

TODO: Describe relationship between between ``Definite integrals'', ``Indefinite integrals'', and ``Antiderivatives''.

%% https://en.wikipedia.org/wiki/Fundamental_theorem_of_calculus
``The fundamental theorem of calculus is a theorem that links the concept of differentiating a function with the concept of integrating a function.

The first part of the theorem, sometimes called the first fundamental theorem of calculus, states that one of the antiderivatives (also called indefinite integral), say F, of some function f may be obtained as the integral of f with a variable bound of integration. This implies the existence of antiderivatives for continuous functions.[1]

Conversely, the second part of the theorem, sometimes called the second fundamental theorem of calculus, states that the integral of a function f over some interval can be computed by using any one, say F, of its infinitely many antiderivatives. This part of the theorem has key practical applications, because explicitly finding the antiderivative of a function by symbolic integration allows for avoiding numerical integration to compute integrals.''

``Newton and Leibniz

The major advance in integration came in the 17th century with the independent discovery of the fundamental theorem of calculus by Newton and Leibniz. The theorem demonstrates a connection between integration and differentiation. This connection, combined with the comparative ease of differentiation, can be exploited to calculate integrals. In particular, the fundamental theorem of calculus allows one to solve a much broader class of problems. Equal in importance is the comprehensive mathematical framework that both Newton and Leibniz developed. Given the name infinitesimal calculus, it allowed for precise analysis of functions within continuous domains. This framework eventually became modern calculus, whose notation for integrals is drawn directly from the work of Leibniz.
Formalization
While Newton and Leibniz provided a systematic approach to integration, their work lacked a degree of rigour. Bishop Berkeley memorably attacked the vanishing increments used by Newton, calling them "ghosts of departed quantities". Calculus acquired a firmer footing with the development of limits. Integration was first rigorously formalized, using limits, by Riemann. Although all bounded piecewise continuous functions are Riemann-integrable on a bounded interval, subsequently more general functions were considered—particularly in the context of Fourier analysis—to which Riemann's definition does not apply, and Lebesgue formulated a different definition of integral, founded in measure theory (a subfield of real analysis). Other definitions of integral, extending Riemann's and Lebesgue's approaches, were proposed. These approaches based on the real number system are the ones most common today, but alternative approaches exist, such as a definition of integral as the standard part of an infinite Riemann sum, based on the hyperreal number system.''

Newton and Leibniz independently discovered the fundemental theorem of calculus.
They based their definitions on infinitesimals which, as described above was considered too imprecise.
Later, Riemann rigorously formalized integration using limits.

``There are many ways of formally defining an integral, not all of which are equivalent. The differences exist mostly to deal with differing special cases which may not be integrable under other definitions, but also occasionally for pedagogical reasons. The most commonly used definitions of integral are Riemann integrals and Lebesgue integrals.''

We only look at Reimann integrals for now, since they will likely be more familiar to most students (They are to me!).

``In the branch of mathematics known as real analysis, the Riemann integral, created by Bernhard Riemann, was the first rigorous definition of the integral of a function on an interval. It was presented to the faculty at the University of Göttingen in 1854, but not published in a journal until 1868.[1] For many functions and practical applications, the Riemann integral can be evaluated by the fundamental theorem of calculus or approximated by numerical integration.

The Riemann integral is unsuitable for many theoretical purposes. Some of the technical deficiencies in Riemann integration can be remedied with the Riemann–Stieltjes integral, and most disappear with the Lebesgue integral.''

See: \href{https://mathoverflow.net/questions/52708/why-should-one-still-teach-riemann-integration}

So there are some problems, but Reimann integrals will do for now.(?)
%% Because we won't do rigorous proofs requiring Lebesgue integrals in this course?

\end{document}
