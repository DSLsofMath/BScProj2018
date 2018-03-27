
Proofs
======

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}

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

Rigorösa namn
-------------

Vi börjar med att rigoröst definera alla ingående namn i kinematikens trevliga värld. I Haskell ska vi koda upp saker, och de mest grundläggande saken är uttryck, och det mest grundläggande uttrycket är namnen vi här presenterar.

Vi ska ha *påståenden* som säger si och så om uttryck. Eftersom påståenden är typer måste även *olika* uttryck vara typer. Det löses av att tillägget `DataKinds` används. `Expr` kommer alltså betraktas som en sort med typer.

![Overview](Overview.png)

Vi tänker i termer av en låda som förflyttar sig längs en axel. Den har olika positioner vi olika tidpunkter. Därför blir $x(t)$, $v(t)$ och $a(t)$ lådans *aktuella* position, hastighet respektive acceleration vid *en viss* tidpunkt $t$. $t$ är ett "tidsindex" som pekar ut en viss tidpunkt.

Vi kodar upp tid och funktionerna

> data Expr = Tfun Expr
>           | Xfun Expr
>           | Vfun Expr
>           | Afun Expr

Det som är funktioner har ett till uttryck som argumentet.

Det här med "final", "initial" och "0" syftar på *specifika* tidpunkter i ett experiment. Initialt och 0 på den initiala tidpunkten och final på den slutgiltiga tidpunkten. Dessa är *fixa* tidpunkter. Bara $f$ och $i$ brukar anges för att syfta på det finala respektive initiala *tillståndet*. I praktiken blir dom t.ex. initial hastighet beroende på vilken storhet man snackar om.

Detta ger följande definerande samband.

\begin{align}
  t_f &= \{\text{Tid vid finalt, när experimentet är slut}\} \\
  t_i &= \{\text{Tid vid initialt, när experimentet startar}\} = t_0 \\
  x_f &= x(t_f) \\
  x_i &= x(t_i) = x_0 \\
  v_f &= v(t_f) \\
  v_i &= v(t_i) = v_0 \\
\end{align}

>           | Tf
>           | Ti
>           | T0
>           | Xf
>           | Xi
>           | X0
>           | Vf
>           | Vi
>           | V0

Varför är inte accelerationen med? Jo, för dessa fyra formler uttnyttjar att accelerationen är konstant. Är $a$ konstant? Vad syftar ens bara $a$ på? I detta sammanhanget menar man mer explcit att 

\begin{align}
  a(t) = a_{value}
\end{align}

där $a_{value}$ är ett *värde*, ett tal. Nu när vad accelerationen är har tydliggjorts så förstår man också varför $a_f$ och $a_i$ är relevanta att ha med.

>           | Avalue

Hur är det med $\Delta$? Hur ska det tolkas? Defintionen av $\Delta$ är *förändring* i tid/position/hastighet. Förändring mellan vad? Det är ju en differens åtminstone. Differens mellan vad? Ja, det är ofta lite löst definerat. Här tänker vi ge det en tydlig definition, nämligen som skillndaden mellan *aktuell* och *ursprunglig*. Det ger att

\begin{align}
  \Delta t &= t - t_i \\
  (\Delta x)(t) &= x(t) - x_i \\
  (\Delta v)(t) &= v(t) - v_i \\
\end{align}

>           | DeltaTfun Expr
>           | DeltaXfun Expr
>           | DeltaVfun Expr

$\Delta$ av något blir en funktion av tiden. Vi skrev också $(\Delta x)(t)$ och inte $\Delta x(t)$. Det är tydligare att låta $\Delta$ syfta på differensen i *storheten* i sig, och inte *funktionen* som beskriver storheten.

Nu är symboliska namn introducerade. Men de är inte de enda uttrycken. Vi behöver också aritmetik

>           | Add Expr Expr
>           | Sub Expr Expr
>           | Mul Expr Expr
>           | Div Expr Expr

Nu har vi gjort namn och samband rigorösa, och namn är uppkodade i `Expr`. Dags att koda upp likheter.

Men det är så här att likheter ska enbart gälla mellan uttryck. Dvs `Equal` är en typkonstruktor som tar två `Expr`.

< data Equal (a :: Expr) (b :: Expr) where

Denna rad gör att vi kan skapa typer som anger likheter mellan olika `Expr`. Vi kan t.ex. skriva

< Equal Ti T0

som är påståendet att `Ti` och `T0` är lika. Men vi behöver kunna beivsa det, dvs skapa ett värde av den typen. Då behövs datakonstruktorer till `Equal`-typen.

En grundläggande liket och därmed ett sätt att skapa värden är

<   Refl :: Equal c c

som säger att något är lika med sig självt.

Det blir så att de datakonstruktorer vi skapar är axiom eftersom de kan skapas från tomma luften.

I vanlig bevisföring i datorn så nöjer man sig med detta och bygger allt på det. Men vi fuskar lite och introducerar kraftfulla axiom.

Utifrån `Refl` som enda axiom kan man bevis allt, men det kräver många steg. Vi utgår från starkare axiom som man "uppenbarligen" vet gäller, t.ex. att addition är kommutativ. Det skulle bli "out of scope" för oss. Vi fokuserar på de bevis som görs i fysiken.

Vi har dessutom de axiom som ska gälla per definiton. Vi börjar med dom.

Här är enkla definerande likheter mellan symboliska namn.

<   Tfinal :: Equal Tf (Tfun Tf)
<   Tinitial :: Equal Ti (Tfun Ti)
<   Tinitial2 :: Equal Ti T0
<   Xfinal :: Equal Xf (Xfun Tf)
<   Xinitial :: Equal Xi (Xfun Ti)
<   Xinitial2 :: Equal Xi X0
<   Vfinal :: Equal Vf (Vfun Tf)
<   Vinitial :: Equal Vi (Vfun Ti)
<   Vinitial2 :: Equal Vi V0

Och nu delta-funktionerna

<   DeltaTeq :: Equal (DeltaTfun t) (Sub (Tfun t) Ti)
<   DeltaXeq :: Equal (DeltaXfun t) (Sub (Xfun t) Xi)
<   DeltaVeq :: Equal (DeltaVfun t) (Sub (Vfun t) Vi)

Och slutligen värdet på accelerationsfunktionen. Den var ju konstant.

<   Acceleration :: Equal (Afun t) Avalue
<   Acceleration2 :: Equal (Afun t) (Div (DeltaVfun t) (DeltaTfun t))

Den första för att accelerationen är konstant. Den andra för att eftersom accelerationen är konstant så är denna kvot samma hela tiden.

-----

Vi ska börja med att bevisa

\begin{align}
  v_f &= v_i + a*t 
\end{align}

Vi ska alltså skapa ett värde av följande typ:

> type Proof1 = Vf `Equal` (Vi `Add` (Avalue `Mul` Tf))

Två viktiga tolkningar vi gjorde var

- $a$ syftar på det konstanta värde `Avalue`
- $t$ syfter på tiden i det finala tillståndet

Att accelerationen är konstant var den första inskränkningen. Den andra är att $t_0 = 0$, dvs vi sätter experimenets startpunkt som refrens. TODO: ta med zero som en likhet tidigare.

**(I)**

Vi utgår ifrån

\begin{align}
  a = \frac{(\Delta v)(t)}{\Delta t}
\end{align}

Vi behöver genast två matematiska likheter, nämliligen symmetri och transitivitet

<   Symmetric :: a `Equal` b -> b `Equal` a
<   Transitive :: a `Equal` b -> b `Equal` c -> a `Equal` c

> s1 :: Avalue `Equal` (DeltaVfun t `Div` DeltaTfun t)
> s1 = Transitive (Symmetric Acceleration) Acceleration2

**(II)**

Kongruens för division behövs

<   DivCong1 :: a `Equal` b -> (a `Div` c) `Equal` (b `Div` c)
<   DivCong2 :: a `Equal` b -> (c `Div` a) `Equal` (c `Div` b)

> s2 :: Avalue `Equal` ((Vfun t `Sub` Vi) `Div` DeltaTfun t)
> s2 = Transitive s1 (DivCong1 DeltaVeq)

> s3 :: Avalue `Equal` ((Vfun t `Sub` Vi) `Div` (Tfun t `Sub` Ti))
> s3 = Transitive s2 (DivCong2 DeltaTeq)

**(III)**
OBS t på båda ställen var godtyckligt, men det var *samma* och det är det viktiga.

Likheten för "upp-multiplicering" behövs nu.

<   MulUpDiv :: a `Equal` (b `Div` c) -> (a `Mul` c) `Equal` b

> s4 :: (Avalue `Mul` (Tfun t `Sub` Ti)) `Equal` (Vfun t `Sub` Vi)
> s4 = MulUpDiv s3


**(V)**

Likheten `s3` gäller för *alla* `t`. Då gäller den speciellt för `t = Tf`.

< s4 :: Avalue `Equal` ((Vfun Tf `Sub` Vi) `Div` (Tfun Tf `Sub` Ti))
< s4 = s3



-------------------

Här ligger de riktiga definitionerna. Så att i löptexten kan göra lite hipp-som-happ

> data Equal (a :: Expr) (b :: Expr) where
>   Refl :: Equal c c

>   Tfinal :: Equal Tf (Tfun Tf)
>   Tinitial :: Equal Ti (Tfun Ti)
>   Tinitial2 :: Equal Ti T0
>   Xfinal :: Equal Xf (Xfun Tf)
>   Xinitial :: Equal Xi (Xfun Ti)
>   Xinitial2 :: Equal Xi X0
>   Vfinal :: Equal Vf (Vfun Tf)
>   Vinitial :: Equal Vi (Vfun Ti)
>   Vinitial2 :: Equal Vi V0

>   DeltaTeq :: Equal (DeltaTfun t) (Sub (Tfun t) Ti)
>   DeltaXeq :: Equal (DeltaXfun t) (Sub (Xfun t) Xi)
>   DeltaVeq :: Equal (DeltaVfun t) (Sub (Vfun t) Vi)

>   Acceleration :: Equal (Afun t) Avalue
>   Acceleration2 :: Equal (Afun t) (Div (DeltaVfun t) (DeltaTfun t))

>   Symmetric :: Equal a b -> Equal b a
>   Transitive :: Equal a b -> Equal b c -> Equal a c

>   DivCong1 :: a `Equal` b -> (a `Div` c) `Equal` (b `Div` c)
>   DivCong2 :: a `Equal` b -> (c `Div` a) `Equal` (c `Div` b)

>   MulUpDiv :: a `Equal` (b `Div` c) -> (a `Mul` c) `Equal` b


---------------



------------------------------





En typ av sort Equal x y är ett bevis och ett värde är då beviset

Men det som är Equal mellan är uttryck. Detta behöver man skilja åt, så kanske ha sort för uttryck och sort för bevis?

Används som typ

< data Expr = Div Expr Expr
<           | Mul Expr Expr
<           | A
<           | V
<           | T

< data Equal (a :: Expr) (b :: Expr) where
<   Refl :: Equal c c
<   Avg :: Equal A (Div V T)

< x = Refl
< y = Avg
< z = Refl :: (Equal V V)

"Ekvivalens" mellan två uttryck ska inte gå. Behöver kanske ha sort för bevis. Bevis här är bara att likheter gäller.

> -- Ett försök att lösa ovanstående
> --data Eqvi (a :: Equal (x :: Expr) (y :: Expr)) (b :: Equal (p :: Expr) (q :: Expr)) where
> --  Self :: Eqvi c c -- x implies x
> --  MulUpDiv :: Eqvi (Equal a (Div b c)) (Equal (Mul a c) b)

< data Eqvi a b where
<   Self :: Eqvi c c -- x implies x
<   MulUpDiv :: Eqvi (Equal a (Div b c)) (Equal (Mul a c) b)

< -- Tar en ekivalens, och dessa ena premiss, 
< -- och skapar dess konsekvens
< transform :: a -> Eqvi a b -> b
< transform = undefined


< reflexive :: Equal a a
< reflexive = Refl


< symetric :: Equal a b -> Equal b a
< symetric Refl = Refl
< -- ?

< transitive :: Equal a b -> Equal b c -> Equal a c
< transitive Refl Refl = Refl
< -- ?


< -- Axiom
< s0 :: Equal A (Div V T)
< s0 = Avg

< s1 :: Equal (Mul A T) V
< s1 = transform s0 MulUpDiv