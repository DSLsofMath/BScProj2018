
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
  t_i &= \{\text{Tid vid initialt, när experimentet startar}\} = t_0 = 0\\
  x_f &= x(t_f) \\
  x_i &= x(t_i) = x_0 \\
  v_f &= v(t_f) \\
  v_i &= v(t_i) = v_0 \\
\end{align}

Man brukar ha som konvention att $t_0 = 0$. Det betyder att experimentets startpunkt sätts som referenstidspunkt.

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
>   Tinitial1 :: Ti `Equal` T0
>   Tinitial2 :: Ti `Equal` Zero

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

>   Reflexive    :: a `Equal` a
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
>   CongPoly2    :: a2 `Equal` a2' -> PolyFun a0 a1 a2 t `Equal`
>                                     PolyFun a0 a1 a2' t

Integraler

>   IntegEval    :: Integ (PolyFun a0   a1 Zero           t') l u t'
>                   `Equal`
>                        ((PolyFun Zero a0 (a1 `Div` Two) u) `Sub`
>                         (PolyFun Zero a0 (a1 `Div` Two) l))

Identiteter

> -- TODO: Bara L, bevisa R
>   ZeroNum      :: (Zero `Div` a) `Equal` Zero
>   ZeroMul      :: (Zero `Mul` a) `Equal` Zero
>   ZeroMulR     :: (a `Mul` Zero) `Equal` Zero
>   ZeroAddL     :: (Zero `Add` a) `Equal` a
>   ZeroAddR     :: (a `Add` Zero) `Equal` a
>   ZeroSub      :: (a `Sub` Zero) `Equal` a

Aritmetik

>   MulDistSub   :: (a `Mul` (b `Sub` c)) `Equal`
>                   ((a `Mul` b) `Sub` (a `Mul` c))
>   AddCom       :: (a `Add` b) `Equal` (b `Add` a)

Polynom

>   PolyEval     :: PolyFun a0 a1 a2 t `Equal` ((a0 `Add` (a1 `Mul` t)) `Add` (a2 `Mul` (t `Mul` t)))

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

Vi beräknar integralen

> dvEavaltSavalti :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue (Zero `Div` Two) t) 
>         `Sub` 
>        (PolyFun Zero Avalue (Zero `Div` Two) Ti))
> dvEavaltSavalti = dvEinaval `Transitivity` IntegEval

\begin{align}
  (\Delta v)(t) = a_{value} * t - a_{value} * t_i
\end{align}

**(IV)**

Vi ska hyfsa den där likheten lite. Semantiskt blir det dock ingen skillnad från steget innan. Först görs `Zero \`Div\` Two` till `Zero`.

> dvEavaltSavalti1 :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue Zero t) 
>         `Sub` 
>        (PolyFun Zero Avalue (Zero `Div` Two) Ti))
> dvEavaltSavalti1 = dvEavaltSavalti `Transitivity` x
>   where
>     x = CongSubL y
>     y = CongPoly2 ZeroNum

och den andra av dem

> dvEavaltSavalti2 :: DeltaVfun t 
>         `Equal` 
>       ((PolyFun Zero Avalue Zero t) 
>         `Sub` 
>        (PolyFun Zero Avalue Zero Ti))
> dvEavaltSavalti2 = dvEavaltSavalti1 `Transitivity` x
>   where
>     x ::  (PolyFun Zero Avalue Zero t `Sub` 
>            PolyFun Zero Avalue (Div Zero Two) Ti)
>          `Equal`
>           (PolyFun Zero Avalue Zero t `Sub` 
>            PolyFun Zero Avalue Zero           Ti)
>     x = CongSubR y
>     y = CongPoly2 ZeroNum

**(V)**

Det är ganska uppenbart för oss människor att

\begin{align}
  0 + a*t + 0*t*t = a*t
\end{align}

men i datorn måste vi ändå göra det fullständigt. Vi inser att det kommer behöva göras flera gånger och vara väldigt omständigt. Så vi gör en liten generell lemma.

> polyLinear :: PolyFun Zero e Zero t `Equal` (e `Mul` t)
> polyLinear = p
>   where
>     x :: PolyFun Zero e Zero t `Equal` 
>          ((Zero `Add` (e `Mul` t)) `Add` (Zero `Mul` (t `Mul` t)))
>     x = PolyEval
>
>     y :: PolyFun Zero e Zero t `Equal` 
>          ((Zero `Add` (e `Mul` t)) `Add` Zero)
>     y = x `Transitivity` (CongAddR ZeroMul)
>
>     z :: PolyFun Zero e Zero t `Equal` (Zero `Add` (e `Mul` t))
>     z = y `Transitivity` ZeroAddR
>
>     p :: PolyFun Zero e Zero t `Equal` (e `Mul` t)
>     p = z `Transitivity` ZeroAddL

Så... vad ska detta användas på? Jo den ska användas för att förbättra hygienen från det föregående steget.

> dvEavaltSavalti3 :: DeltaVfun t `Equal` ((Avalue `Mul` t) `Sub`
>                                          (Avalue `Mul` Ti))
> dvEavaltSavalti3 = y
>   where
>     x = dvEavaltSavalti2 `Transitivity` CongSubL polyLinear
>     y = x                `Transitivity` CongSubR polyLinear

**(VI)**

Nu är vi nästan klara.

> dvEavalMtSti :: DeltaVfun t `Equal` (Avalue `Mul` (t `Sub` Ti))
> dvEavalMtSti = dvEavaltSavalti3 `Transitivity` 
>                (Symmetry MulDistSub)

\begin{align}
  (\Delta v)(t) = a_{value} * (t - t_i)
\end{align}

**(VII)**

Vi vecklar upp vänsterledet

> vfunSviEavalMtSti :: (Vfun t `Sub` Vi) `Equal` 
>                      (Avalue `Mul` (t `Sub` Ti))
> vfunSviEavalMtSti = (Symmetry DeltaVdef) `Transitivity`
>                     dvEavalMtSti

och flyttar över `Vi`

...

Ah! Vi har inget axiom för det. Vi kan bara addera `Vi` på båda sidor och sedan kancellera det på VL. Låt oss bevisa ett lemma.

> subToAdd :: (a `Sub` b) `Equal` c -> a `Equal` (c `Add` b)
> subToAdd aSbEc = y
>   where
>     --x :: ((a `Sub` b) `Add` b) `Equal` (c `Add` b)
>     x = CongAddL aSbEc
>     y = (Symmetry AddSub) `Transitivity` x

Vi nyttjar lemmat

> vfunEavalMtStiAvi :: Vfun t `Equal` ((Avalue `Mul` (t `Sub` Ti)) `Add` Vi)
> vfunEavalMtStiAvi = subToAdd vfunSviEavalMtSti

\begin{align}
  v(t) = a_{value} * (t - t_i) + v_i
\end{align}

**(VIII)**

Vi använder konventionen $t_i = 0$.

> vFunEavalMtAvi :: Vfun t `Equal` ((Avalue `Mul` t) `Add` Vi)
> vFunEavalMtAvi = vfunEavalMtStiAvi `Transitivity` x
>   where
>     x :: ((Avalue `Mul` (Sub t Ti)) `Add` Vi) `Equal` 
>          ((Avalue `Mul` t         ) `Add` Vi)
>     x = CongAddL y
>     y :: (Avalue `Mul` (t `Sub` Ti)) `Equal` 
>          (Avalue `Mul` t           )
>     y = CongMulR z
>     z :: (t `Sub` Ti) `Equal` t
>     z = r `Transitivity` ZeroSub
>     q :: (t `Sub` Ti) `Equal` (t `Sub` Ti)
>     q = Reflexive
>     r :: (t `Sub` Ti) `Equal` (t `Sub` Zero)
>     r = q `Transitivity` CongSubR Tinitial2

\begin{align}
  v(t) = a_{value} * t + v_i
\end{align}

Phew! Så där. Nu kan vi känna oss nöja med detta första bevis.

Andra beviset
-------------

Vi ska nu bevisa

\begin{align}
  x(t) = x_i + v_i * t + \frac{a_{värde}*t^2}{2}
\end{align}

**(I)**

Vi börjar där precis som innan med ett axiom

> dxtEinv :: DeltaXfun t `Equal` Integ (Vfun t') Ti t t'
> dxtEinv = DeltaXint

\begin{align}
  (\Delta x)(t) = \int_{t_i}^t v(t) dt
\end{align}

**(II)**

Eftersom integraler är inblandade behöver vi omvandla `Vfun t` till ett polynom.

> vfun :: Vfun t `Equal` ((Avalue `Mul` t) `Add` Vi)
> vfun = vFunEavalMtAvi

> vfun' :: Vfun t `Equal` ((Vi `Add` (Avalue `Mul` t)) `Add` 
>                         (Zero `Mul` (t `Mul` t)))
> vfun' = vfun `Transitivity` x
>   where
>     x :: ((Avalue `Mul` t) `Add` Vi) `Equal` 
>          ((Vi `Add` (Avalue `Mul` t)) `Add` 
>          (Zero `Mul` (t `Mul` t)))
>     x = Symmetry s
>     y :: ((Avalue `Mul` t) `Add` Vi) `Equal` 
>          ((Avalue `Mul` t) `Add` Vi)
>     y = Reflexive
>     z :: ((Avalue `Mul` t) `Add` Vi) `Equal` 
>          (Vi `Add` (Avalue `Mul` t))
>     z = y `Transitivity` AddCom
>     q :: (((Avalue `Mul` t) `Add` Vi) `Add` 
>             (Zero `Mul` (t `Mul` t))) `Equal` 
>          ((Vi `Add` (Avalue `Mul` t)) `Add` 
>             (Zero `Mul` (t `Mul` t)))
>     q = CongAddL z
>     r :: ((Vi `Add` (Avalue `Mul` t)) `Add` 
>           (Zero `Mul` (t `Mul` t))) `Equal` 
>          (((Avalue `Mul` t) `Add` Vi) `Add` Zero) 
>     r = (Symmetry q) `Transitivity` (CongAddR ZeroMul)
>     s :: ((Vi `Add` (Avalue `Mul` t)) `Add` 
>           (Zero `Mul` (t `Mul` t))) `Equal` 
>          ((Avalue `Mul` t) `Add` Vi) 
>     s = r `Transitivity` ZeroAddR

Nu kan vi göra till en polynomfunktion.

> vfunPoly :: Vfun t `Equal` PolyFun Vi Avalue Zero t
> vfunPoly = vfun' `Transitivity` (Symmetry PolyEval)

**(III)**

Vi återvänder till det första steget och omvandlar till polynomfunktion

> dxtEinvfp :: DeltaXfun t `Equal` Integ (PolyFun Vi Avalue Zero t') Ti t t'
> dxtEinvfp = dxtEinv `Transitivity` CongInteg vfunPoly

**(IV)**

Integralen beräknas

> dxtEpSp :: (DeltaXfun t) `Equal` 
>         ((PolyFun Zero Vi (Avalue `Div` Two) t) `Sub`
>         (PolyFun Zero Vi (Avalue `Div` Two) Ti))
> dxtEpSp = dxtEinvfp `Transitivity` IntegEval

**(V)**

Vi gör ett lemma för att omvandla polynomfunktionen

> polyCubeLin :: PolyFun Zero a1 a2 t `Equal` 
>                ((a1 `Mul` t) `Add` (a2 `Mul` (t `Mul` t)))
> polyCubeLin = x `Transitivity` y
>   where
>     x :: PolyFun Zero a1 a2 t `Equal` 
>          ((Zero `Add` (a1 `Mul` t)) `Add` (a2 `Mul` (t `Mul` t)))
>     x = PolyEval
>     y :: ((Zero `Add` (a1 `Mul` t)) `Add` 
>           (a2 `Mul` (t `Mul` t))) `Equal` 
>          ((a1 `Mul` t) `Add` (a2 `Mul` (t `Mul` t)))
>     y = CongAddL ZeroAddL

och så använder vi den

> -- Cubic Linear
> type CL a1 a2 t = (a1 `Mul` t) `Add` (a2 `Mul` (t `Mul` t))
> -- Avalue Half
> type AH = Avalue `Div` Two

> s5 :: DeltaXfun t `Equal`
>       (CL Vi AH t
>      -- (Add (Mul Vi t) (Mul (Div Avalue Two) (Mul t t)))
>           `Sub`
>        CL Vi AH Ti)
>      -- (Add (Mul Vi Ti) (Mul (Div Avalue Two) (Mul Ti Ti))))
> s5 = y
>   where
>     x = dxtEpSp `Transitivity` CongSubL polyCubeLin
>     y = x `Transitivity` CongSubR polyCubeLin

\begin{align}
  (\Delta x)(t) = v_i * t + \frac{a_{value}*t^2}{2} - (v_i * t_i + \frac{a_{value}*t_i^2}{2})
\end{align}

**(VI)**

Vi vill nyttja konventionen $t_i = 0$ igen nu.

> toZero :: CL a1 a2 Ti `Equal` Zero
> toZero = u
>   where
>     x :: ((a1 `Mul` Ti) `Add` (a2 `Mul` (Ti `Mul` Ti))) `Equal`
>          ((a1 `Mul` Ti) `Add` (a2 `Mul` (Ti `Mul` Ti)))
>     x = Reflexive
>     y :: (Zero `Add` (a2 `Mul` (Ti `Mul` Ti))) `Equal`
>          ((a1 `Mul` Ti)   `Add` (a2 `Mul` (Ti `Mul` Ti)))
>     y = CongAddL p
>     z :: (c `Mul` Zero) `Equal` (c `Mul` Ti)
>     z = CongMulR (Symmetry Tinitial2)
>     p :: Zero `Equal` (c `Mul` Ti)
>     p = Symmetry ZeroMulR `Transitivity` z
>
>     q :: (a2 `Mul` (Ti `Mul` Ti)) `Equal` (a2 `Mul` (Ti `Mul` Zero))
>     q = CongMulR (CongMulR Tinitial2)
>     
>     r = (CongMulR ZeroMulR)
>     s = (q `Transitivity` r) `Transitivity` ZeroMulR
>     t = Symmetry y `Transitivity` CongAddR s
>     u = t `Transitivity` ZeroAddL

> s6 :: DeltaXfun t `Equal` CL Vi AH t
> s6 = (s5 `Transitivity` CongSubR toZero) `Transitivity` ZeroSub

\begin{align}
  (\Delta x)(t) = v_i * t + \frac{a*t^2}{2}
\end{align}















































