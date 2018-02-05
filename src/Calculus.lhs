\documentclass[12pt,a4paper]{article}

\usepackage{fontspec}

%%% Standard definitions from the lhs2TeX installation
%include polycode.fmt

%include Calculus.format

\begin{document}

\title{Calculus}

\maketitle

\section{Introduction}
Calculus is cool

Differences, derivatives, and integrals

\section{Differences}

Differences are used for stuff like average velocity.

$$ v_{avg} = \frac{x_2 - x_1}{t_2 - t_1} =\frac{\Delta x}{\Delta t} $$

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
>           | Lambda String Expr -- Lambda function
>           | Delta Expr         -- Difference
>           | Expr :$ Expr      -- Function application
>   deriving (Show, Eq)

> avg (y1, x1) (y2, x2) = (y2 :- y1) :/ (x2 :- x1)

is equivalent to

> avg' y x t1 t2 = ((y :$ t2) :- (x :$ t1)) :/ ((x :$ t2) :- (x :$ t1))

which is equivalent to

> avg'' y x = Delta y :/ Delta x

> data Val = RealVal RealNum | LambdaVal [(String, Val)] String Expr
>   deriving (Show, Eq)

> eval :: [(String, Val)] -> Expr -> Val
> eval env (Const x) = RealVal x
> eval env (a :+ b) = evalBinop env a b (:+) (+)
> eval env (a :- b) = evalBinop env a b (:-) (-)
> eval env (a :* b) = evalBinop env a b (:*) (*)
> eval env (a :/ b) = evalBinop env a b (:/) (/)
> eval env (Var s) = fromMaybe (error ("Variable " ++ s ++ " is not in environment: " ++ show env)) (lookup s env)
> eval env (Lambda p b) = LambdaVal env p b

$ \Delta x = f $ where $ f(t1, t2) = x(t2) - x(t1)

> eval env (Delta x) = LambdaVal env "_t1" (Lambda "_t2" ((x :$ (Var "_t2")) :- (x :$ (Var "_t1"))))
> eval env (((Var "sin") :$ arg)) = RealVal (sin (evalReal env arg))
> eval env ((f :$ arg)) = case (eval env f) of
>     LambdaVal lenv p b -> eval ((p, eval env arg) : lenv) b
>     _ -> error "Not a function"

> evalBinop env a b cons op = case (eval env a, eval env b) of

Arithmetic on real numbers is just as normal

>     (RealVal a', RealVal b') -> RealVal (a' `op` b')

A nice definition for function (addition/subtraction/...) that works for differentials
$ f + g = h $ where $ h(x) = f(x) + g(x) $

>     (LambdaVal lenv1 p1 b1, LambdaVal lenv2 p2 b2) ->
>         LambdaVal (lenv1 `union` lenv2) "_x" (((Lambda p1 b1) :$ (Var "_x")) `cons` ((Lambda p2 b2) :$ (Var "_x")))

> evalReal :: [(String, Val)] -> Expr -> RealNum
> evalReal env e = case (eval env e) of
>     RealVal x -> x
>     _         -> error "Wrong type of value. Expected RealVal"

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

\section{Integrals}

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

\end{document}
