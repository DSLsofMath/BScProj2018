> module Introduction.GettingStarted where

What you need to dive in
======================================================================

To just follow along and implement the same stuff we do, a single
document loaded into GHCi will do. For this, you just need to install the
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
when you load a module in GHCi.



If you need to sharpen up your skills
======================================================================

If you need to freshen up on your Haskell, [Learn You a Haskell for
Great Good](http://learnyouahaskell.com/) is a great book that covers
all the important stuff in a humorous manner that really holds your
attention.

If you still don't really get what DSLs are all about, try reading the
["notes" (almost a full book really) for the course on DSLs of math at
Chalmers](https://github.com/DSLsofMath/DSLsofMath/tree/master/L/snapshots). If
you're studying at Chalmers, it is even better to actually take the course!

If there's some part of the material that you still think is unclear
(or maybe even wrong? Blasphemy!), please [open an issue on the github
repo!](https://github.com/DSLsofMath/BScProj2018/issues). We'll look
into it!
