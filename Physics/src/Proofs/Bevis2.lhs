
Oskars anteckningar
-------------------

Vissa regler är generella, som upp-multiplicering

Andra är specifika, som a = dv/dt. Ha data A osv för att skilja på dom

Värden och typer olika saker
Det inuti equals är namn på värden
Equals-värde i sig är sats/bevis att gäller mellan just dessa

Likheter kan moddas, tex a=b/c till c*a=b. "Har ett påstående av denna form, ja då kan vi skapa ett nytt påstående"

Uttryck kan moddas, tex a*b till b*a.

När likheter, påståenden, moddas, så är det ekvivalenser som nyttjas.

När uttryck moddas, så är det likheter som nyttjas.

För den sistnämnda, ha både funktion och Equals-värde. Även equalsElim: tar värde och likhet, ger andra värdet.

Fast kanske bäst att bara ha transitivitet. 

Ekvationer Oskar haft i åtanke
------------------------------

Utgå ifrån

\begin{align}
  v &= \frac{dx}{dt} \\
  a &= \frac{dv}{dt} \\
\end{align}

Om accelerationen är konstant så visa att följande gäller.

\begin{align}
  v_f &= v_i + a*t \\
  x_f &= x_i + 0.5*(v_f + v_i)*t \\
  x_f &= x_i + v_i*t + 0.5*a*t^2 \\
  v_f^2 &= v_i^2 + 2*a*(x_f - x_i) \\
\end{align}

Faktiska starten
----------------

Vi ska bevisa grejer genom att manipulera ekvationer. En ekvation är en likhet mellan två uttryck.

> data Equals lhs rhs

Ett uttryck kan vara en variabel. Variabler vi kommer behöva är...

> data A -- Acceleration
> data V -- Hasighet
> data T -- Tid

Ett uttryck kan också vara en multiplikation eller addition av två andra uttryck.

> data Mul a b
> data Div num den

En typ av uttryck är en differential av ett uttryck.

> data Diff x

Det finns likheter mellan uttryck med specifika variabler.

> s0 :: Equals A (Div (Diff V) (Diff T))
> s0 = undefined

`s0` är en likhet vi antar gäller, vi tar den som ett axiom. Notera att den bara gäller för variablerna `A`, `V` och `T`. Följande skulle vara felaktigt

< s0 :: Equals a (Div (Diff v) (Diff t))
< s0 = undefined

eftersom det skulle gälla för *alla* `a`, `v` och `t`, vilket det inte gör, t.ex. `a=A`, `v=T` och `t=V`.

Däremot finns det likheter som gäller för *alla* uttryck. T.ex.

> mulCom :: Equals (Mul a b) (Mul b a)
> mulCom = undefined

Det finns något snarlikt *likheter*, nämligen *ekvivalenser*. En likhet är att två *uttryck* är utbytbara medan en ekvivalens är att två *satser* är utbytbara. En likhet är en sats.

Ett exempel på en ekvivalens är

> mulUpDiv :: Equals a (Div b c) -> Equals (Mul a c) b
> mulUpDiv = undefined

Man kan tänka att en ekvivalens ropar ut till omvärlden: "Har du ett bevis att <något på denna formen> gäller, och ger det till mig, kan du få ett annat bevis på denna form."

Detta är faktiskt det första vi ska göra.

> s1 :: Equals (Mul A (Diff T)) (Diff V)
> s1 = mulUpDiv s0

Gör man samma operation på båda sidor av lika-med-tecknet är det tillåtet. Specifikt är att integera på båda sidor tillåtet.

Vad är det för slags integral vi vill göra egentligen? Jo, vi vill integrera "från initialt tillstånd till finalt tillstånd", "i -> f". Beroende på om det integreras map tid eller hastighet, så är det från initial tid till final tid, eller initial hastighet till final hastighet. Dvs, det man får ut av integralen är $t_f-t_i$, eller $\Delta t$. Vi börjar med $\Delta$ tankesättet.

> data Integ a
> data Delta x

Nu är det en ekvivalens som gäller, eftersom det är en likhett som transformers till en annan likhet. (I andra ord, ett påstående till ett annat påstående).

> bothSideInteg :: Equals a b -> Equals (Integ a) (Integ b)
> bothSideInteg = undefined

TODO: Kräv att indata är differentialer

TODO: Vi kanske måste ha något sätt att markera att variabler är konstanta.

Vi integrerar likheten på båda sidor.

> s2 :: Equals (Integ (Mul A (Diff T))) (Integ (Diff V))
> s2 = bothSideInteg s1

Integralen av en ensam differential är delta.

> eqLoneDiffDelta :: Equals (Integ (Diff x)) (Delta x)
> eqLoneDiffDelta = undefined

Den likhet vill vi applicera på `s2`-likheten. Tur att likheter är transitiva, en form av ekvivalens.

> transitivity :: Equals a b -> Equals a c -> Equals b c
> transitivity = undefined

Likheter är också kommutativa.

> equCom :: Equals a b -> Equals b a
> equCom = undefined

> s3 :: Equals (Integ (Diff V)) (Integ (Mul A (Diff T)))
> s3 = equCom s2

> s4 :: Equals (Integ (Mul A (Diff T))) (Delta V)
> s4 = transitivity s3 eqLoneDiffDelta

En integral av en multiplikation av en konstant och en differential, är en multiplikation av konstant och integralen av differentialen.

TODO: Hur vet att den är en konstant?

> eqMoveOutMult :: Equals (Integ (Mul a (Diff b))) (Mul a (Integ (Diff b)))
> eqMoveOutMult = undefined

> s5 :: Equals (Delta V) (Mul A (Integ (Diff T)))
> s5 = transitivity s4 eqMoveOutMult

> transitivityInMul :: Equals a (Mul b c) -> Equals c d -> Equals a (Mul b d)
> transitivityInMul = undefined

> s6 :: Equals (Delta V) (Mul A (Delta T))
> s6 = transitivityInMul s5 eqLoneDiffDelta

TODO: Typer för tids-beroende och oberoende variabler. För final och initiala värden.
