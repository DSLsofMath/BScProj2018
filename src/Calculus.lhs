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

$$ v_{avg-x} = \frac{\Delta x}{\Delta t} = \frac{x_2 - x_1}{t_2 - t_1} $$

> delta x = \(t1, t2) -> x t2 - x t1

\section{Derivatives}

Derivatives are used for stuff like instantaneous velocity.

$$ v_x = \frac{dx}{dt} = lim_{\Delta t \to 0} \frac{\Delta x}{\Delta t} $$

\section{Integrals}

Integrals are used in the reversed way as derivatives.

$$ x_{traveled} = \int_{t_0}^{t_1} v(t) dt $$

\end{document}
