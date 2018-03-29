
Proofs
======

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}

> module Proofs.Proofs
> (
> )
> where

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
  t_i &= \{\text{Tid vid initialt, när experimentet startar}\} = t_0 = 0\\
  x_f &= x(t_f) \\
  x_i &= x(t_i) = x_0 \\
  v_f &= v(t_f) \\
  v_i &= v(t_i) = v_0 \\
\end{align}

En konvention är att låta $t_i = 0$. Det betyder att experimentets startpunkt blir tidsreferenspunkt.

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


Uppkodning av namnen och sambanden
----------------------------------

Som vi såg finns det två komponenter: *uttryck* och *likheter mellan uttryck*. Att två uttryck är lika är ett påstående, och likheter behöver alltså vara typer. Därför måste även uttryck vara typer.

Vi börjar med att göra en *sort* med *typer* för uttryck

< data Expr =

Med tillägget `DataKinds` blir detta inte bara en typ med värden, utan även en sort med typer.

Sedan gör vi en *typ* som representerar likhet mellan två andra typer, och dessa typer måste tillhöra sorten `Expr`.

< data Equal (a :: Expr) (b :: Expr) where

**Uttryck**

Vad för slags uttryck finns det? Till att börja med addition, subtraktion, multiplikation och divison olika uttryck.

> data Expr = Expr `Add` Expr
>           | Expr `Sub` Expr
>           | Expr `Mul` Expr
>           | Expr `Div` Expr

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

Vi har värdet på vad accelerationen är

>           | Avalue

Till sist har vi $\Delta$-funktionerna

>           | DeltaTfun Expr
>           | DeltaXfun Expr
>           | DeltaVfun Expr

**Definerande likheter**

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

Vi har även två intressanta likheter när det kommer till accelerationen. Först

>   AfunCon :: Afun t `Equal` Avalue

som säger att funktionen för acceleration, *för alla* `t`, är lika med `Avalue`.

Den andra likheten är

>   AfunQuo :: Afun t `Equal` (DeltaVfun t `Div` DeltaTfun t)

som säger att accelerationen, för alla `t`, är lika med kvoten mellan $(\Delta V)(t)$ och $\Delta T$. Detta gäller eftersom accelerationen är konstant.

Till sist har vi likheterna som definerar $\Delta$-funktionerna

>   DeltaXdef :: DeltaXfun t `Equal` (Xfun t `Sub` Xi)
>   DeltaVdef :: DeltaVfun t `Equal` (Vfun t `Sub` Vi)
>   DeltaTdef :: DeltaTfun t `Equal` (t `Sub` Ti)

Okej, *allra* sist har vi konventionen att $t_i = 0$

>   ConvT     :: Ti `Equal` Zero

**Matematiska likheter**

Okej, så nu har vi de definerande likheterna. Vi behöver även några matematiska likheter. Man skulle kunna tänka sig att definera *alla* som finns, bara för att det inte ska verka som att vi plockar ut det vi behöver, men det skulle bli väldigt många. Så vi låtsas vara lata och bara definerar några som råkar vara de vi behöver.

Är detta inte fusk? I vanlig bevisföring i datorn nöjer man sig med en enda grundläggande likhet `Refl` och bevisar *allt* utifrån den. Det skulle här vara "out-of-scope". Vi skaffar kraftfulla axiom så att vi kan fokusera på de bevis som görs i fysiken. Axiomen kommer "uppenbarligen" vara rätt.

De vi behöver blir...

>   Symmetry     :: a `Equal` b -> b `Equal` a
>   Transitivity :: a `Equal` b -> b `Equal` c -> a `Equal` c
>   CongMul      :: a `Equal` b -> (a `Mul` c) `Equal` (b `Mul` c)
>   MulDiv1      :: ((b `Div` a) `Mul` a) `Equal` b
>   CongAdd      :: a `Equal` b -> (a `Add` c) `Equal` (b `Add` c)
>   AddSub1      :: ((b `Sub` a) `Add` a) `Equal` b
>   Cong         :: a `Equal` b -> (f a) `Equal` (f b)
>   Cong2        :: (f a b) `Equal` (f a c) -> (f b a) `Equal` (f c a)
>   Cong3        :: a `Equal` b -> (f a) c `Equal` (f b) c
>   Hej          :: a `Equal` b -> ((f a) c) `Equal` ((f b) c)

> test :: a `Equal` b -> (Add c a) `Equal` (Add c b)
> test = Cong

> --test2 :: a `Equal` b -> (f a c) `Equal` (f b c)
> --test2 = Hej

> test3 :: a `Equal` b -> ((Add a) c) `Equal` ((Add b) c)
> test3 = Hej

> --test2 :: a `Equal` b -> ((Add a) c) `Equal` ((Add b) c)
> --test2 aEb = Cong2 hej
> --  where
> --    hej = test aEb

> --hej :: a `Equal` b -> 

> skoj :: f a b -> f b a
> skoj = flip

> type family Flip (f :: Expr -> Expr -> Expr) (b :: Expr) (c :: Expr) where
>   Flip f b a = f a b

> type Ad a b = Flip Add a b

> --test2 :: a `Equal` b -> ((Add a) c) `Equal` ((Add b) c)
> --test2 = Cong3

> --test3 :: a `Equal` b -> ((Add c) a) `Equal` ((Add c) b)
> --test3 = Cong3

> --hej :: (a -> b) -> f a -> f b
> --hej _ a = a

> congAdd :: a `Equal` b -> (c `Add` a) `Equal` (c `Add` b)
> congAdd = Cong

> --congMul :: a `Equal` b -> (a `Add` c) `Equal` (b `Add` c)
> --congMul = Cong2

> hej :: Ti `Equal` T0
> hej = Tinitial

> hej2 :: ('Add Tf Ti) `Equal` ('Add Tf T0)
> hej2 = Cong hej

> --hej3 = Cong2 hej2


> --test :: f -> a -> f a
> --test = undefined

Första beviset
--------------

Vi ska börja med att bevisa

\begin{align}
  v_f = v_i + a*t 
\end{align}

som mer rigoröst bör skrivas som

\begin{align}
  v_f = v_i + a_{value} * \Delta t
\end{align}

Vi ska alltså skapa ett värde av följande typ:

< type Proof1 = Vf `Equal` (Vi `Add` (Avalue `Mul` DeltaTfun))

Nu kör vi!

**(I)**

Vi börjar med

> afunEdvDdt :: Afun t `Equal` (DeltaVfun t `Div` DeltaTfun t)
> afunEdvDdt = AfunQuo

\begin{align}
  a(t) = \frac{(\Delta v)(t)}{(\Delta t)(t)}
\end{align}

**(II)**

Vi använder transitivitet för att relatera kvoten till det aktuella värdet på accelerationen.

> avalEdvDdt :: Avalue `Equal` (DeltaVfun t `Div` DeltaTfun t)
> avalEdvDdt = afunEaval `Transitivity` afunEdvDdt
>   where
>     afunEaval :: Avalue `Equal` Afun t
>     afunEaval = Symmetry AfunCon

\begin{align}
  a_{value} = \frac{(\Delta v)(t)}{(\Delta t)(t)}
\end{align}

**(III)**

Vi multiplicerar bägge sidor med högerledets kvot

> avalMdtEdvDdtMdt :: (Avalue `Mul` DeltaTfun t) `Equal` ((DeltaVfun t `Div` DeltaTfun t) `Mul` DeltaTfun t)
> avalMdtEdvDdtMdt = CongMul avalEdvDdt

\begin{align}
  a_{value} * (\Delta t)(t) = \frac{(\Delta v)(t)}{(\Delta t)(t)} * (\Delta t)(t)
\end{align}

**(IV)**

Vi förenklarar högerledet

> avalMdtEdv :: (Avalue `Mul` DeltaTfun t) `Equal` DeltaVfun t
> avalMdtEdv = avalMdtEdvDdtMdt `Transitivity` MulDiv1

\begin{align}
  a_{value} * (\Delta t)(t) = (\Delta v)(t)
\end{align}

**(V)**

Vi splittrar högerledet

> avalMdtEvfSvi :: (Avalue `Mul` DeltaTfun t) `Equal` (Vfun t `Sub` Vi) 
> avalMdtEvfSvi = avalMdtEdv `Transitivity` DeltaVdef

\begin{align}
  a_{value} * (\Delta t)(t) = v(t) - v_i
\end{align}

**(VI)**

och flyttar över `Vi`

> avalMdtAviEvfunSviAvi :: ((Avalue `Mul` DeltaTfun t) `Add` Vi) `Equal` ((Vfun t `Sub` Vi) `Add` Vi)
> avalMdtAviEvfunSviAvi = CongAdd avalMdtEvfSvi

> avalMdtAviEvfun :: ((Avalue `Mul` DeltaTfun t) `Add` Vi) `Equal` (Vfun t)
> avalMdtAviEvfun = avalMdtAviEvfunSviAvi `Transitivity` AddSub1

\begin{align}
  a_{value} * (\Delta t)(t) + v_i = v(t)
\end{align}

**(VII)**

Och slutligen byter sida

> vfunEavalMdtAvi :: (Vfun t) `Equal` ((Avalue `Mul` DeltaTfun t) `Add` Vi) 
> vfunEavalMdtAvi = Symmetry avalMdtAviEvfun

\begin{align}
  v(t) = a_{value} * (\Delta t)(t) + v_i
\end{align}

Detta är en välbekant formel. Den säger att den aktuella hastighetnen är lika med det konstanta accelerationsvärdet gånger tiden sedan experimentet startade pluss den initiala hastighen. Rimligt va?

Vi ska masera uttrycket lite också.

**(VIII)**

Först splittras 

Om man uttnyttjar konventionen att $t_i = 0$ får man

