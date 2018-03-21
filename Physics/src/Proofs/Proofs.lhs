
Proofs
======

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}

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

Vi börjar med att rigoröst definera alla ingående namn i kinematikens trevliga värld. I Haskell ska vi koda upp saker, och de mest grundläggande saken är uttryck, och det mest grundläggande uttrycket är namnen vi här presenterar. Vi börjar skriva

< data Expr = ...

Vi ska ha *påståenden* som säger si och så om uttryck. Eftersom påståenden är typer måste även *olika* uttryck vara typer. Det löses av att tillägget `DataKinds` används.

Vi ska även koda upp *likheter* mellan uttryck. Till det använder vi nedanstående GADT.

> data Equal (a :: Expr) (b :: Expr) where


Vardan denna komplexa och skumma grej? Jo, det är så här att en *likhet* gäller mellan två uttryck. Och uttryck är en typ av sorten `Expr`. Så vi vill tvinga `Equal x y` typen att `x` och `y` ska vara just uttryck, och inte vad som helst.

En grundläggande likhet är

>   Refl :: Equal c c

HÄR ÄR JAG NU



Nå, låt oss komma till själva fysiken.

![Overview](Overview.png)

Vi tänker i termer av en låda som förflyttar sig längs en axel. Den har olika positioner vi olika tidpunkter. Därför blir $x(t)$, $v(t)$ och $a(t)$ lådans *aktuella* position, hastighet respektive acceleration vid *en viss* tidpunkt $t$. $t$ är ett "tidsindex" som pekar ut en viss tidpunkt.

Vi kodar upp tid och funktionerna

> data Expr = T
>           | Xfun
>           | Vfun
>           | Afun

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

Nu när alla namn på saker och ting rigoröst blivit definerade, och även sambanden mellan dom, kan vi börja koda upp det i Haskell.








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


> reflexive :: Equal a a
> reflexive = Refl


> symetric :: Equal a b -> Equal b a
> symetric Refl = Refl
> -- ?

> transitive :: Equal a b -> Equal b c -> Equal a c
> transitive Refl Refl = Refl
> -- ?


< -- Axiom
< s0 :: Equal A (Div V T)
< s0 = Avg

< s1 :: Equal (Mul A T) V
< s1 = transform s0 MulUpDiv