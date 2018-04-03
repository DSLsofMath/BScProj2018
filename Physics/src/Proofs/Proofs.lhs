
Proofs
======

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}

> module Proofs.Proofs where
> --(
> --)
> --where

Detta kapitel ska förklara och bevisa fyra grundläggande kinematiska formler. De är som följer

\begin{align}
  v_f &= v_i + a*t \\
  x_f &= x_i + \frac{v_f + v_i}{2}*t \\
  x_f &= x_i + v_i*t + \frac{a*t^2}{2} \\
  v_f^2 &= v_i^2 + 2*a*(x_f - x_i) \\
\end{align}

De gäller om $a$ är konstant. Men vad *exakt* syftar alla namn på? Och vad har $\Delta$ med dessa att göra? I samband med att vi kodar upp dem kommer detta göras rigoröst och tydligt.

Bevisen sker i Haskell mha av Curry Howard korrespondensen. Den säger att påståenden är typer och bevis är värden. Det betyder att om man skapar ett värde av en viss typ så har man ett bevis för det påståendet.

Rigorösa namn och definerade samband
------------------------------------

Det första vi ska göra att rigröst definera vad olika namn betyder och hur de hänger ihop.

![Overview](Overview.png)

Vi tänker i termer av en låda som förflyttar sig längs en axel. Den har olika positioner vi olika tidpunkter. Därför blir $x(t)$, $v(t)$ och $a(t)$ lådans *aktuella* position, hastighet respektive acceleration vid *en viss* tidpunkt $t$. $t$ är ett "tidsindex" som pekar ut en viss tidpunkt.

Det här med "final", "initial" och "0" syftar på *specifika* tidpunkter i ett experiment. Initialt och 0 på den initiala tidpunkten och final på den slutgiltiga tidpunkten. Dessa är *fixa* tidpunkter. Bara $f$ och $i$ brukar anges för att syfta på det finala respektive initiala *tillståndet*. I praktiken blir dom t.ex. initial hastighet beroende på vilken storhet man snackar om.

Detta ger följande definerande samband.

\begin{align}
  t_f &= \{\text{Tid vid finalt, när experimentet är slut}\} \\
  t_i &= \{\text{Tid vid initialt, när experimentet startar}\} = t_0\\
  x_f &= x(t_f) \\
  x_i &= x(t_i) = x_0 \\
  v_f &= v(t_f) \\
  v_i &= v(t_i) = v_0 \\
\end{align}

Varför är inte accelerationen med? Jo, för dessa fyra formler uttnyttjar att accelerationen är konstant. Är $a$ konstant? Vad syftar ens bara $a$ på? I detta sammanhanget menar man mer explcit att 

\begin{align}
  a(t) = a_{value}
\end{align}

där $a_{value}$ är ett *värde*, ett tal. Nu när vad accelerationen är har tydliggjorts så förstår man också varför $a_f$ och $a_i$ är relevanta att ha med.

Hur är det med $\Delta$? Hur ska det tolkas? Defintionen av $\Delta$ är *förändring* i tid/position/hastighet. Förändring mellan vad? Det är ju en differens åtminstone. Differens mellan vad? Ja, det är ofta lite löst definerat. Här tänker vi ge det en tydlig definition, nämligen som skillndaden mellan *aktuell* och *ursprunglig*. Det ger att

\begin{align}
  \Delta t &= t - t_i \\
  (\Delta x)(t) &= x(t) - x_i \\
  (\Delta v)(t) &= v(t) - v_i \\
\end{align}

$\Delta$ av något blir en funktion av tiden. Vi skrev också $(\Delta x)(t)$ och inte $\Delta x(t)$. Det är tydligare att låta $\Delta$ syfta på differensen i *storheten* i sig, och inte *funktionen* som beskriver storheten.

För $\Delta$ finns två andra likheter, nämligen

\begin{align}
  (\Delta x)(t) &= \int_{t_i}^t v(t) dt \\
  (\Delta v)(t) &= \int_{t_i}^t a(t) dt \\
\end{align}

vilket man kan förstå om man tittar på bilden

TODO: bild


Uppkodning av namnen och sambanden
----------------------------------

Som vi såg finns det två komponenter: *uttryck* och *likheter mellan uttryck*. Att två uttryck är lika är ett påstående, och likheter behöver alltså vara typer. Därför måste även uttryck vara typer.

Vi börjar med att göra en *sort* med *typer* för uttryck

< data Expr =

Med tillägget `DataKinds` blir detta inte bara en typ med värden, utan även en sort med typer.

Sedan gör vi en *typ* som representerar likhet mellan två andra typer, och dessa typer måste tillhöra sorten `Expr`.

< data Equal (a :: Expr) (b :: Expr) where

Uttryck
-------

Vad för slags uttryck finns det? Till att börja med addition, subtraktion, multiplikation och divison olika uttryck.

> data Expr = Expr `Add` Expr
>           | Expr `Sub` Expr
>           | Expr `Mul` Expr
>           | Expr `Div` Expr

Det är också integration

>           | Integ Expr Expr Expr Expr

där argumenten har följande betydelse

1. Uttryck att integrera
2. Undre gräns
3. Övre gräns
4. Vad som integreras map

En annan typ av uttryck är de symboliska namn som definerades innan. Det är funktionerna som anger position, hastighet och acceleration.

>           | Xfun Expr
>           | Vfun Expr
>           | Afun Expr

Dessa typer har ett argument. Det argumentet är ett annat uttryck som är tiden funktionen ska evalueras i.

Vi har även de olika finala och initiala värdena.

>           | X0
>           | Xi
>           | Xf
>           | V0
>           | Vi
>           | Vf
>           | T0
>           | Ti
>           | Tf

Här passar vi att flika in några tal vi kommer behöva.

>           | Zero
>           | Two

Vi har värdet på vad accelerationen är

>           | Avalue

Till sist har vi $\Delta$-funktionerna

>           | DeltaTfun Expr
>           | DeltaXfun Expr
>           | DeltaVfun Expr

Inte riktigt trots allt. Ett slags *uttryck* är en generell polynomfunktion. Detta för att kunna behandla alla funktioner lika vid integration. Sedan råkar vi bara behöva polynomfunktioner, och inte exempelvis sinus-funktioner.

>           | PolyFun Expr -- a0
>                     Expr -- a1
>                     Expr -- a2
>                     Expr -- t

Är en polynomfunktion som ser ut som

\begin{align}
  p(t) = a0 + a1 * t + a2 * t^2
\end{align}

Definerande likheter
--------------------

Det var alla uttryck vi behöver. Nu ska vi koda upp de likheter som behövs. Likheterna är av två slag. De som är definerande, alltså anger vad någon symbol betyder samt olika matematiska likheter, t.ex. att addition är kommutativ.

> data Equal (x :: Expr) (y :: Expr) where

Bland de definerande likheterna har vi de som relaterar initiala och finala lägen.

>   Xinitial1 :: Xi `Equal` Xfun Ti
>   Xinitial2 :: X0 `Equal` Xfun Ti
>   Xfinal    :: Xf `Equal` Xfun Tf
>   Vinitial1 :: Vi `Equal` Vfun Ti
>   Vinitial2 :: V0 `Equal` Vfun Ti
>   Vfinal    :: Vf `Equal` Vfun Tf
>   Tinitial  :: Ti `Equal` T0

Vi har även likheten för acceleration

>   AfunCon :: Afun t `Equal` PolyFun Avalue Zero Zero t

som säger att funktionen för acceleration, *för alla* `t`, är lika med `Avalue`.

Vi har likheterna som definerar $\Delta$-funktionerna

>   DeltaXdef :: DeltaXfun t `Equal` (Xfun t `Sub` Xi)
>   DeltaVdef :: DeltaVfun t `Equal` (Vfun t `Sub` Vi)
>   DeltaTdef :: DeltaTfun t `Equal` (t `Sub` Ti)

och likheterna som relaterar $\Delta$-funktionerna till integraler

>   DeltaXint :: DeltaXfun t `Equal` Integ (Vfun t') Ti t t'
>   DeltaVint :: DeltaVfun t `Equal` Integ (Afun t') Ti t t'

**Viktigt:** *alla* funktioner (t.ex. `DeltaXfun` och `Vfun`) behöver kunna uttryckas som en polynomfunktion för att vara explicita. Men bara `Afun` har en likhet med polynomfunktion som axiom. Alla andra är bara relaterade till varandra på olika vis.

Matematiska likheter
--------------------

Okej, så nu har vi de definerande likheterna. Vi behöver även några matematiska likheter. Man skulle kunna tänka sig att definera *alla* som finns, bara för att det inte ska verka som att vi plockar ut det vi behöver, men det skulle bli väldigt många. Så vi låtsas vara lata och bara definerar några som råkar vara de vi behöver.

Är detta inte fusk? I vanlig bevisföring i datorn nöjer man sig med en enda grundläggande likhet `Refl` och bevisar *allt* utifrån den. Det skulle här vara "out-of-scope". Vi skaffar kraftfulla axiom så att vi kan fokusera på de bevis som görs i fysiken. Axiomen kommer "uppenbarligen" vara rätt.

De vi behöver blir...

Egenskaper hos likhet

>   Symmetry     :: a `Equal` b -> b `Equal` a
>   Transitivity :: a `Equal` b -> b `Equal` c -> a `Equal` c

Canceling out

>   MulDiv       :: ((b `Div` a) `Mul` a) `Equal` b
>   AddSub       :: ((b `Sub` a) `Add` a) `Equal` b

Kongruenser

>   CongAddL     :: a `Equal` b -> (a `Add` c) `Equal` (b `Add` c)
>   CongAddR     :: a `Equal` b -> (c `Add` a) `Equal` (c `Add` b)
>   CongSubL     :: a `Equal` b -> (a `Sub` c) `Equal` (b `Sub` c)
>   CongSubR     :: a `Equal` b -> (c `Sub` a) `Equal` (c `Sub` b)
>   CongMulL     :: a `Equal` b -> (a `Mul` c) `Equal` (b `Mul` c)
>   CongMulR     :: a `Equal` b -> (c `Mul` a) `Equal` (c `Mul` b)
>   CongInteg    :: a `Equal` b -> Integ a x y z `Equal` 
>                                  Integ b x y z

Integraler

>   IntegEval    :: Integ (PolyFun a0   a1 Zero           t') l u t'
>                   `Equal`
>                        ((PolyFun Zero a0 (a1 `Div` Two) u) `Sub`
>                         (PolyFun Zero a0 (a1 `Div` Two) l))

Första beviset
--------------

Vi ska börja med att bevisa

\begin{align}
  v_f = v_i + a*t 
\end{align}

som mer rigoröst bör skrivas som

\begin{align}
  v_f = v_i + a_{value} * (\Delta t)(t_f)
\end{align}

Vi ska alltså skapa ett värde av följande typ:

< type Proof1 = Vf `Equal` (Vi `Add` (Avalue `Mul` DeltaTfun Tf))

Nu kör vi!

**(I)**

Vi börjar med två axiom, definerande likhetheter.

> afunEaval :: Afun t `Equal` PolyFun Avalue Zero Zero t
> afunEaval = AfunCon

> dvEinafun :: DeltaVfun t `Equal` Integ (Afun t') Ti t t'
> dvEinafun = DeltaVint

\begin{align}
  a(t) = a_{value} && (\Delta v)(t) = \int_{t_i}^t a(t) dt
\end{align}

**(II)**

Vi ersätter integralen av acceleration som funktion med acceleration som värde.

> dvEinaval :: DeltaVfun t `Equal` Integ (PolyFun Avalue Zero Zero t') Ti t t'
> dvEinaval = dvEinafun `Transitivity` x
>   where
>     x :: Integ (Afun t) l u t' `Equal` 
>          Integ (PolyFun Avalue Zero Zero t) l u t'
>     x = CongInteg afunEaval

\begin{align}
  (\Delta v)(t) = \int_{t_i}^t a_{value} dt
\end{align}

**(III)**

> s3 :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue (Zero `Div` Two) t) 
>         `Sub` 
>        (PolyFun Zero Avalue (Zero `Div` Two) Ti))
> s3 = dvEinaval `Transitivity` IntegEval



















































