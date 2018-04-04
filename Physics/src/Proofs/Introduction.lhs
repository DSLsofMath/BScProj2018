
WIP

Introduction
============

> module Proofs.Introduction
> (
> )
> where

This chapter is about proving stuff in Haskell. More specifically, you'll learn lots about basic kinematics so that it can be defined rigorously, how to encode it in Haskell and finally how to prove it.

We'll to de proofs using the *Curry-Howard correspondence*, which says that types and statements and values are proofs.

![Some fancy Curry-Howard grafitti](CHC.png)

That is, to *state* something is to write *a type*. If the statement is true, a *proof* of it is a *value* if the correct type. The value must be constructed using the axioms and inference rules defined in the given context. Sounds abstract? Well, unfortunely you'll just have to trust me that it'll become clear in the following chapters.

So what is this kinematics you're talking about? Have a look at these formulas

\begin{align}
  v_f &= v_i + a*t \\
  x_f &= x_i + \frac{v_f + v_i}{2}*t \\
  x_f &= x_i + v_i*t + \frac{a*t^2}{2} \\
  v_f^2 &= v_i^2 + 2*a*(x_f - x_i) \\
\end{align}

and you should know what I'm talking about. It's the basic fundamental newtonian kinematic formulas for one-dimensional movement. This chapter focuses on finding out what they really mean so that we can prove them.

However, proving equlities is not the *easiest* thing to prove in Haskell. Therefire, to get a feeling of the Curry-Howard correspondce, we'll warm up by proving some basic logic. Afterwards we'll present the kinematic forumulas in great detail to clear out all ambiguitus. And finally we'll prove two of those formulas.

Let's begin!


































