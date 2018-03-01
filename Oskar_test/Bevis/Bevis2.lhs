
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