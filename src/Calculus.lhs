% -*- xelatex -*-

\documentclass[12pt,a4paper]{article}

\usepackage{fontspec}

%%% Standard definitions from the lhs2TeX installation
%include polycode.fmt

\begin{document}

\title{Calculus}

\maketitle

\section{Introduction}
Calculus is cool

Differentials, derivatives, and integrals

\section{Differentials}

Differentials are used for stuff like average velocity.

$$ v_{avg} = \frac{x_2 - x_1}{t_2 - t_1} =\frac{\Delta x}{\Delta t} $$

> import Data.Maybe

> data Expr = R Double
>           | Sub Expr Expr
>           | Div Expr Expr
>           | Var String
>           | Lambda String Expr
>           | Delta Expr
>           | App Expr Expr

> avg (y1, x1) (y2, x2) = (y2 `Sub` y1) `Div` (x2 `Sub` x1)

is equivalent to

> avg' y x t1 t2 = (App y t2 `Sub` App x t1) `Div` (App x t2 `Sub` App x t1)

which is equivalent to

> avg'' y x = Delta y `Div` Delta x

> eval :: [(String, Expr)] -> Expr -> Double
> eval env (R x) = x
> eval env (Sub a b) = eval env a - eval env b
> eval env (Div a b) = eval env a / eval env b
> eval env (App (Div (Delta y) (Delta x)) arg) =
>     eval env (App (Lambda "t1" (Lambda "t2" (Div ((App y (Var "t2") `Sub` (App y (Var "t1"))))
>                                                  ((App x (Var "t2") `Sub` (App x (Var "t1")))))))
>                   arg)
> eval env (App (Lambda p b) x) = eval ((p, x) : env) b
> eval env (Var s) = eval env (fromJust (lookup s env))

\section{Derivatives}

Derivatives are used for stuff like instantaneous velocity.

$$ v_x = \frac{dx}{dt} = lim_{\Delta t \to 0} \frac{\Delta x}{\Delta t} $$

\section{Integrals}

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

\end{document}
