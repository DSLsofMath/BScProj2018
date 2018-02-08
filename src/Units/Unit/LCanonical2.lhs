
\documentclass[12pt,a4paper]{article}

\usepackage[utf8]{inputenc}
\usepackage[a4paper, margin=1in]{geometry}
\usepackage{verbatim}

\setlength{\parindent}{0pt}
\setlength{\parskip}{\baselineskip}

\usepackage{color}
\definecolor{color_keyword}{rgb}{0,0.0,0.6}
\usepackage{inconsolata}

\usepackage{listings}
%\lstnewenvironment{code}{\lstset{language=Haskell}}{}
\lstset{language=Haskell}
\lstset{basicstyle=\ttfamily,breaklines=true,keywordstyle=\color{color_keyword}}

\lstnewenvironment{code}{}{}
\lstnewenvironment{spec}{}{}

\long\def\ignore#1{}

\begin{document}

\section{Start på ett DSL för enheter}

Det här avsnittet handlar om ett domänspecifikt språk för enheter. Först lite Haskell-teknikaliteter.

\begin{code}
{-# LANGUAGE InstanceSigs #-}

module Unit.LCanonical
( Unit(..)
, BaseUnit(..)
, length
, time
, mass
, one
)
where

import Prelude hiding (length)
import Data.List hiding (length)

import Helper
\end{code}

Inom fysik finns enbart 7 så kallade grundenheter. Men bara 3 av dessa är relevanta för \textit{Fysik för ingenjörer}.

\begin{code}
data BaseUnit = Length
              | Time
              | Mass
              deriving (Eq, Ord)
\end{code}

Enheter i allmänhet kan som bekant vara kombinationer av grundenheterna. Det kanoniska sättet att representera dem är en lista med tupler.

\begin{code}
newtype Unit = Unit [(BaseUnit, Int)]
\end{code}

I varje tupel är första element en grundenhet, och det andra exponentialen. T.ex. har $m^2$ exponentialen 2. Några exempel på enheter m.h.a detta DSL kommer nedan.

\begin{code}
length :: Unit
length = Unit [(Length, 1)]

area :: Unit
area = Unit [(Length, 2)]

time :: Unit
time = Unit [(Time, 1)]

mass :: Unit
mass = Unit [(Mass, 1)]

velocity :: Unit
velocity = Unit [(Length, 1), (Time, -1)]

acceleration :: Unit
acceleration = Unit [(Length, 1), (Time, -2)]
\end{code}

När man tänker på en enhet som acceleration är det kanske inte naturligt att tänka sig den som en multiplikation av meter och tid upphöjt till minus 2! Men det är trots allt samma sak enligt potensreglerna. Och detta sätt att representera enheter på är ''kanoniskt'' eftersom det går att skriva dem entydigt. Det är också enkelt att skriva kod för att manipulera enheter av detta slag, och skriva ut dem snyggt.

En skalär, som t.ex. friktionskoefficienter, är enhetslösa. De representers av tomma listan.

\begin{code}
one :: Unit
one = Unit []
\end{code}

\section{Kanonifiering}

Tidigare nämndes det att nuvarande sätt att representera enheter gör att de går att skriva entydigt. Så låt oss göra en funktion som gör just det!

\begin{code}
canonify :: Unit -> Unit
canonify (Unit []) = Unit []
canonify (Unit us) = Unit . reverse $ sorted'
  where
    sorted  = sort us
    grouped = groupBy (\(u1, _) (u2, _) -> u1 == u2) sorted
    summed  = map f grouped
    nonZero = filter (\g@(_, n) -> n /= 0) summed
    sorted' = sortBy (\(_, n1) (_, n2) -> compare n1 n2) nonZero
    
    f us = let (u, _) = head us
               nTot   = sum $ map (\(_, n) -> n) us
           in (u, nTot)
\end{code}

För att testa funktionen gör vi en ''skum'' enhet.

\begin{code}
weird :: Unit
weird = Unit [(Length, 2), (Length, 1), (Time, -3), (Length, -3), (Mass, 2)]
\end{code}

\ignore{

\begin{code}
weird' :: Unit
weird' = canonify weird

weird2 :: Unit
weird2 = canonify $ Unit [(Length, 2), (Time, -4), (Mass, -2)]
\end{code}

}

\section{Pretty-printer}

Det nämndes att med en kanonisk representation kunde enheter kunna skrivas ut snyggt. Det gör vi här.

\begin{code}
instance Show BaseUnit where
  show Length = "m"
  show Time   = "s"
  show Mass   = "kg"

instance Show Unit where
  show = showUnit
\end{code}

Grundenheterna skrivs kort och gott ut som SI-enheten.

Hjälpfunktion för att skriva ut en grundenhet med exponent.

\begin{code}
showUnitTuple :: (BaseUnit, Int) -> String
showUnitTuple (u, 1) = show u
showUnitTuple (u, n) = show u ++ "^" ++ show n
\end{code}

Och slutligen den stora funktionen som skriver ut.

\begin{code}
showUnit :: Unit -> String
showUnit (Unit []) = ""
showUnit (Unit us)
  | null negStrs = posStr
  | otherwise    = posStr ++ "/" ++ negStr'
  where
    pos = filter (\(_, n) -> n > 0) us
    neg = filter (\(_, n) -> n < 0) us
    neg' = map (\(u, n) -> (u, -n)) neg
    
    posStrs = map showUnitTuple pos
    negStrs = map showUnitTuple neg'
    
    posStr = if null posStrs
             then ""
             else foldl (\strs str -> str ++ "*" ++ strs) 
                        (head posStrs) 
                        (tail posStrs)
    (left, right) = if len negStrs > 1
                    then ("(", ")")
                    else ("", "")
    negStr = if null negStrs
             then ""
             else foldl (\strs str -> str ++ "*" ++ strs) 
                        (head negStrs) 
                        (tail negStrs)
    negStr' = left ++ negStr ++ right
\end{code}

Den intresserade läsaren kan grunna över hur funktionen funkar. Det viktiga är dock att det \textit{går} att gör en sådan här funktion relativt enkelt.

\begin{comment}
------------------------------------------------------------
-- Instansiering

-- Så att de vanliga räknesätten kan användas

instance Eq Unit where
  (==) :: Unit -> Unit -> Bool
  u1 == u2 = u1' == u2'
    where
      (Unit u1') = canonify u1
      (Unit u2') = canonify u2

instance Num Unit where
  u1 + u2
    | u1 == u2 = u1
    | otherwise = error "Units that are added must be the same"
  (Unit u1) * (Unit u2) = canonify . Unit $ u1 ++ u2      
  negate = id
  abs = id
  signum = id
  fromInteger _ = one

instance Fractional Unit where
  recip (Unit u) = canonify . Unit $ map (\(u', n) -> (u', -n)) u
  fromRational _ = one

instance Floating Unit where
  pi    = one
  exp   = id
  log   = id
  sin   = id
  cos   = id
  asin  = id
  acos  = id
  atan  = id
  sinh  = id
  cosh  = id
  asinh = id
  acosh = id
  atanh = id






------------------------------------------------------------
-- Papperssyntax

-- Nu går det att skriva in som man gör på papper
-- och sedan få ett snyggt resultat

vel  = length / time
acc1 = length / time / time
acc2 = length / (time * time)
acc3 = vel / time

-- Grejen är dock att hur det händer sker i "bakom kulisserna"
-- och då kanske poängen med ett DSL för pedagogik försvinner

\end{comment}


\end{document}
