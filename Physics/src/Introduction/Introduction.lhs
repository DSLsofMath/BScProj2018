> module Introduction.Introduction where
> 
> message = "Greetings!"

About this tutorial
======================================================================

You've arrived at **Learn You a Physics for Great Good**,
the #1 stop for all your beginner-to-intermediate needs of learning
**Physics** through the use of **Domain Specifc Languages**
(hereinafter DSLs (mostly but not always)).

This book was written as a [bachelor's thesis
project](https://github.com/DSLsofMath/BScProj2018) at Chalmers
University of Technology as an offshoot of a bachelor's level elective
course [*Domain Specific Languages of
Mathematics*](https://github.com/DSLsofMath/DSLsofMath). The goal of
the the project and the reason for the existance of this book, is to
help primarily CS students learn physics better. We think that the use
of domain specific languages to teach the subject will help set these
students in the right mindset to learn physics efficiently and
properly. An observed problem has been that students base their mental
models completely or partially on intuition and incorrect assumptions,
instead of on definitions and proved theorems where validity is
ensured. This may be a bad habit stemming from the way math and
physics is taught in earlier stages of the education, and we think
that DSLs will inherently force students into the right mindset for
learning this subject well! Further, many textbooks on physics are
incredibly thick and boring, so we've decided to emulate the great and
good [Learn You a Haskell for Great
Good](http://learnyouahaskell.com/) in order to provide som comic
relief in between all the dramatic definitions.

In this book, we will study physics through the lens of DSLs. We will
need to pay close attention to definitions, throughly ponder any
non-trivial syntax, and prove that the things we do are actually
correct. The areas covered include such things as: dimensional
analysis, vectors, calculus, and more!

The book is aimed at you who have some knowledge of
[Haskell](https://www.haskell.org/). If you know what a `class` and an
`instance` is, you're probably good to go! Even if you don't we're
sure you could pick it up as we go. We believe in you!



So what's a DSL?
======================================================================

In general, a domain specific language is simply a computer language
for a specific domain. It's NOT a synonym for
[jargon](http://www.catb.org/jargon/html/online-preface.html)! DSLs
can be specialized for markup, like
[*HTML*](https://en.wikipedia.org/wiki/HTML); for modeling, like
[*EBNF*](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form);
for programming, like
[*Bash*](https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29); and
more.

The languages we will construct will mostly concern modeling of
physics and mathematics. We will create data structures in Haskell to
represent the same physics calculations that we write on paper, in
such a way that we can write functions to, for example, analyze the
expressions for validity.

Look, we'll demonstrate. Let's say we want to model a language that is
a subset to the common algebra we're all familiar with. Our language
will consist expressions of a single variable and addition. For
example, the following three expressions are all valid in such a language:

$$x + x$$

$$x$$

$$x + x + x + x$$

When implementing a DSL, we typically start with modeling the
syntax. Let's first declara a data type for the language

> data Expr

Then we interpret the textual description of the language. "Our
language will consist expressions of a single variable and
addition". Ok, so an expression can be one of two things then: a
single variable

>   = X

or two expressions added together.

>   | Add Expr Expr

And that's it, kind of! A DSL without any associated functions for
validation, symbolic manipulation, evaluation, or somesuch, is really
no DSL at all! We must DO something with it, or there is no point!

One thing we can do with expressions such as these, is compare whether
two of them are equal. Even without using any numbers, we can test
this by simply counting the $x$s! If both expressions contain the same
amount of $x$s, they will be equal!

> eq :: Expr -> Expr -> Bool
> eq e1 e2 = count e1 == count e2
>   where count X           = 1
>         count (Add e1 e2) = count e1 + count e2

We can now test whether our expressions are equal.

< ghci> eq (Add (Add X X) X) (Add X (Add X X))
< True

And NOW that's it (if we want to stop here)! This is a completely
valid (but boring) DSL. We've modeled the syntax, and added a function
that operates symbolically on our language. This is a very small and
simple DSL, and you've likely done something similar before without
even realizing you were constructing a DSL. It can really be that
simple

In other DSLs we may also look at evaluation and the semantics of a
language, i.e. what is the actual type of the result when we
*evaluate* or "compute" our expressions.

Throughout this book we'll construct and use DSLs in many different
ways. Different parts of the physics we look at will require different
DSL treatments, basically. There is no *one* model to use for all
DSLs.



What you need to dive in
======================================================================

To just follow along and implement the same stuff we do, a single
document loaded into GHCI will do. For this, you just need to install the
[Haskell Platform](https://www.haskell.org/platform/).

If you want to automatically install any required dependencies, like
[*Hatlab*](https://github.com/DSLsofMath/Hatlab) which will be used
later on, install [Stack](https://haskellstack.org). Stack is a build
system that will automatically get dependencies and build the project
for you. If you want to build the code the same way we do, this is
what you'll need. With Stack installed and [our repository
cloned](https://github.com/DSLsofMath/BScProj2018), enter the
`Physics` directory and type `stack build`. Stack should now download
and compile all necessary dependencies, such that they are avaiable
when you load a module in GHCI.



If you need to sharpen up your skills
======================================================================

If you need to freshen up on your Haskell, [Learn You a Haskell for
Great Good](http://learnyouahaskell.com/) is a great book that covers
all the important stuff in a humorous manner that really holds your
attention.

If you still don't really get what DSLs are all about, try reading the
["notes" (almost a full book really) for the course on DSLs of math at
Chalmers](https://github.com/DSLsofMath/DSLsofMath/tree/master/L/snapshots). If
you're studying at Chalmers, even better to actually take the course!

If there's some part of the material that you still think is unclear
(or maybe even wrong? Blasphemy!), please [open an issue on the github
repo!](https://github.com/DSLsofMath/BScProj2018/issues). We'll look
into it!
