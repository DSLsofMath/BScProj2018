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

> import Data.Maybe
> import Data.List

A real number. Double is mostly an adequate representation

> type RealNum = Double

The syntax tree of an expression

> data Expr = Const RealNum      -- Real constant
>           | Expr :+ Expr       -- Plus (Addition)
>           | Expr :- Expr       -- Minus (Subtraction)
>           | Expr :* Expr       -- Times (Multiplication)
>           | Expr :/ Expr       -- Divided by (Division)
>           | Var String         -- Variable
>           | Func Func          -- Builtin function
>           | Lambda String Expr -- Lambda function
>           | Delta Expr         -- Difference, like "Δx"
>           | D Expr             -- Differential, like "dx"
>           | Expr :$ Expr       -- Function application
>   deriving (Show, Eq)

> type Func = String

> funcs = [("sin", RealVal . sin . fromRealVal),
>          ("abs", RealVal . abs . fromRealVal),
>          ("signum", RealVal . signum . fromRealVal)]

We implement Num for Expr to make it nicer to usepackage

> instance Num Expr where
>       a + b = a :+ b
>       a - b = a :- b
>       a * b = a :* b
>       abs e = Func "abs" :$ e
>       signum e = Func "signum" :$ e
>       fromInteger = Const . fromInteger

> avg (y1, x1) (y2, x2) = (y2 :- y1) :/ (x2 :- x1)

is equivalent to

> avg' y x t1 t2 = ((y :$ t2) :- (x :$ t1)) :/ ((x :$ t2) :- (x :$ t1))

which is equivalent to

> avg'' y x = Delta y :/ Delta x

> data Val = RealVal RealNum | LambdaVal [(String, Val)] String Expr | FuncVal Func
>   deriving (Show, Eq)

> fromRealVal (RealVal x) = x

> eval :: [(String, Val)] -> Expr -> Val
> eval env (Const x) = RealVal x
> eval env (a :+ b) = evalBinop env a b (:+) (+)
> eval env (a :- b) = evalBinop env a b (:-) (-)
> eval env (a :* b) = evalBinop env a b (:*) (*)
> eval env (a :/ b) = evalBinop env a b (:/) (/)
> eval env (Var s) =
>     fromMaybe (error ("Variable " ++
>                       s ++
>                       " is not in environment: " ++
>                       show env))
>               (lookup s env)
> eval env (Lambda p b) = LambdaVal env p b
> eval env (Func f) = FuncVal f
> eval env ((f :$ arg)) = case (eval env f) of
>     LambdaVal lenv p b -> eval ((p, eval env arg) : lenv) b
>     FuncVal f          -> (fromJust (lookup f funcs)) (eval env arg)
>     _                  -> error "Not a function"
> eval env (Delta x) =
>     LambdaVal env
>               "_a"
>               (Lambda "_b"
>                       ((x :$ (Var "_b")) :- (x :$ (Var "_a"))))

> evalBinop env a b cons op = case (eval env a, eval env b) of

Arithmetic on real numbers is just as normal

>     (RealVal a', RealVal b') -> RealVal (a' `op` b')

A nice definition for function (addition/subtraction/...) that works for
differentials: $ f + g = h $ where $ h(x) = f(x) + g(x) $

>     (LambdaVal lenv1 p1 b1, LambdaVal lenv2 p2 b2) ->
>         LambdaVal (lenv1 `union` lenv2)
>                   "_x"
>                   ((cons ((Lambda p1 b1) :$ (Var "_x"))
>                          ((Lambda p2 b2) :$ (Var "_x"))))

> evalReal :: [(String, Val)] -> Expr -> RealNum
> evalReal env e = fromRealVal (eval env e)

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

We express our definition of \Delta in Haskell:

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
<                       ((x :$ (Var "_b")) :- (x :$ (Var "_a"))))

\subsection{Verification/proof/test}

???

\subsection{Examples}

> x = Lambda "t" ((Var "t") :* (Const 5))
> id' = Lambda "x" (Var "x")
> t = id'
> vAvg = Lambda "x" (Delta (Var "x") :/ Delta t)
> vAvgX = vAvg :$ x
> v = eval [] (vAvgX :$ (Const 0) :$ (Const 10))

\section{Derivatives}

Derivatives are used for stuff like instantaneous velocity.

$$ v_x = \frac{dx}{dt} = lim_{\Delta t \to 0} \frac{\Delta x}{\Delta t} $$

We add the infinitesimal syntax to the \textit{Expr} syntax tree.

<           | D Expr             -- Differential, like "dx"

Leibniz's notation definition. Used to be defined as ``the quotient of an infinitesimal increment of y by an infinitesimal increment of x'':

$$ D(f) = \frac{dy}{dx} = \frac{lim_{\Delta y \to 0} \Delta y}{lim_{\Delta x \to 0} \Delta x} $$

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

\subsection{Verification/proof/test}

???

\subsection{Examples}

\section{Integrals}

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

Debug:

> env = []

\end{document}
