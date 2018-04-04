
WIP

Introduction
============

> module Proofs.Introduction
> (
> )
> where

Vad bevisförining i datorn/Haskell är

Börja med satslogik för att bekanta sig

Sedan presentera kinematik rigoröst och koda upp namnen

Sedan bevisa en formel

Sedan bevisa en annan formel

Avslutning


Detta kapitel ska förklara och bevisa fyra grundläggande kinematiska formler. De är som följer

\begin{align}
  v_f &= v_i + a*t \\
  x_f &= x_i + \frac{v_f + v_i}{2}*t \\
  x_f &= x_i + v_i*t + \frac{a*t^2}{2} \\
  v_f^2 &= v_i^2 + 2*a*(x_f - x_i) \\
\end{align}

De gäller om $a$ är konstant. Men vad *exakt* syftar alla namn på? Och vad har $\Delta$ med dessa att göra? I samband med att vi kodar upp dem kommer detta göras rigoröst och tydligt.

Bevisen sker i Haskell mha av Curry Howard korrespondensen. Den säger att påståenden är typer och bevis är värden. Det betyder att om man skapar ett värde av en viss typ så har man ett bevis för det påståendet.

































