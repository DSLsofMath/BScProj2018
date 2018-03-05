
< data Equals lhs rhs

< data A
< data V
< data T

Variabler kan vara konstanta

< data Const x

eller vara en funktion av en annan variabel.

v = dx/dt

Har sett att vf = vi + a*t

< data Poly1 n a b

Där är `n` "namnet" på variablen, och `a` och `b` är a+b*x uttrycket.

< data Mul a b
< data Div num den

< data Diff x

< time = Poly1 T 

"Givna likheter"
- a(t) = a (annat a som är ett konstant tal)
- vi = v0
- ti = 0
- tf = t(t)
- vf = v(t)

< data V

`v` syftar på hastigheten av lådan.

< data A

`a` syftar på accelerationen hos lådan.

< data T

`t` syftar på "tiden", den absoluta tiden, som klockor visar. Men som en differens från någon tidpunkt. Differensen från experimentets start till "universum skapelse".

Det finala tillståndet är vid `tf`.

< data Final s
< data Initial s

En differential syfter på "vilken del av funktionen som helst", men det är för alla differentialer *samma* bit som syftas på samtidigt.

-------------

När man skriver

\begin{align}
  a = \frac{dv}{dt}
\end{align}

menar man lite mer precist

\begin{align}
  a(t) = \frac{dv(t)}{dt}
\end{align}

. Och när man skriver $a(t)$ menar man lådans acceleration vid ett visst ögonblick, där ögonblicket specificieras av tiden. Samma sak gäller för $v(t)$ fast för hastighet.

$t$ är tids*differensen* sedan en, för experimentet, definerad starttid.

Låt oss koda upp dessa insikter. Först typer för att referera till $a$ och $v$ hos lådan och $t$ hos experimentet.

> data A
> data V
> data T

Sedan en typ för att representera en differential av "något".

> data Diff x

Alla inblandade differentialer hänvisar till samma "bit" av funktionerna.

Och även division av två uttryck.

> data Div num den

Och även likhet mellan två uttryck.

> data Equals lhs rhs

Den likhet vi ska utgå ifrån, och som vi därmed tar som *axiom* är

> s0 :: Equals A (Div (Diff V) (Diff T))
> s0 = undefined

Nästa steg är att skriva om det som

\begin{align}
  dv(t) = a(t) * dt
\end{align}

Så vi introducerar multiplikation

> data Mul a b

och en *ekvivalens* som tar oss från en likhet till en annan likhet.

> mulUpDiv :: (Equals a (Div b c)) -> (Equals b (Mul a c))
> mulUpDiv = undefined

och vi applicerar den direkt.

> s1 :: Equals (Diff V) (Mul A (Diff T))
> s1 = mulUpDiv s0

Nästa steg är att integrera uttrycket på båda sidorna. Man integrerar från $i$ till $f$.

\begin{align}
  \int_i^f dv(t) = \int_i^f a(t) * dt
\end{align}

> data Integ x

> twoSideInteg :: Equals a b -> Equals (Integ a) (Integ b)
> twoSideInteg = undefined

> s2 :: Equals (Integ (Diff V)) (Integ (Mul A (Diff T)))
> s2 = twoSideInteg s1

Att integrera bara en integral på det ovanstående sättet ger

\begin{align}
  \int_i^f dv(t) &= v_f - v_i \\
  v_f &= v(t_f) \\
  v_i &= v(t_i) \\
\end{align}

> data Final n
> data Initial n

> data Sub a b

> integLoneDiff :: Equals (Integ (Diff var)) (Sub (Final var) (Initial var))
> integLoneDiff = undefined

För att nyttja likhehten så använder vi oss av likheters transitivitet

> transitivity :: Equals a b -> Equals a c -> Equals b c
> transitivity = undefined

> s3 :: Equals (Sub (Final V) (Initial V)) (Integ (Mul A (Diff T)))
> s3 = transitivity integLoneDiff s2

Hur tacklas den andra integralen? Det beror på vad för slags "grej" `A` är. Är den en konstant händer en sak. Är det en linjär funktion av $t$ händer något annat o.s.v.

Insikten är att se att `A` (eller $a$) bara är ett namn på en funktion. Vi representerar en funktion på följande vis

> data Poly c2 c1 c0

I `Poly` är funktionen på formen

\begin{align}
  var(t) = c_2*t^2 + c_1*t + c_0
\end{align}

eftersom det kommer visa sig att vi enbart behöver polynomfunktioner av högst andra grad.

I detta bevis har vi det *givet* att accelerationen är konstant. Alltså att $a(t)=K$ där $K$ är något tal, som är den konstanta accelerationens värde.

Det är lätt att blanda ihop $a$ som i $a(t)$ för accelerationen. Och $a$ som i *värdet* på accelerationen, $K$. Vi döper $K$ till $a_{värde}$ för att ha något liknande som $a$, men för att explicit tala om att det är *värdet* den syftar på.

> data A_value

Vi modellerar att "accelerationen är konstant" genom att ha följande likhet

> constantAcc :: Equals A (Poly Zero Zero A_value)
> constantAcc = undefined

Den räknas som "premiss" till uppgiften.

Vi behöver ha talet 0!

> data Zero

Vi behöver använda transitivitet inuti multiplikation

< transitivityMul :: Equals a (Mul b c) -> Equals b d ->  Equals a (Mul d c)
< transitivityMul = undefined

Den ovanstående behöver inte ses som ett axoim. Vi ska visa den! Men först behövs några andra axiom (av slaget ekvivalens)

> divDownMul :: Equals a (Mul b c) -> Equals (Div a c) b
> divDownMul = undefined

> equCom :: Equals a b -> Equals b a
> equCom = undefined

> transitivity' :: Equals b a -> Equals a c -> Equals b c
> transitivity' = transitivity . equCom

Nedanstående form behövs visst inte längre...

< transitivityMul :: Equals a (Mul b c) -> Equals b d -> Equals a (Mul d c)
< transitivityMul aEEbMc bEEd = aEEdMc
<   where
<     aDcEEb = divDownMul aEEbMc
<     bEEaDc = equCom aDcEEb
<     aDcEEd = transitivity bEEaDc bEEd
<     dEEaDc = equCom aDcEEd
<     aEEdMc = mulUpDiv dEEaDc

TODO: Förklara här typed holes och hur man tänkar när man bevisar så här.

> transitivityMul :: Equals a b -> Equals (Mul a c) (Mul b c)
> transitivityMul = undefined

Vi behöver också liknande för integral

> transitivityInteg :: Equals a (Integ b) -> Equals b c -> Equals a (Integ c)
> transitivityInteg = undefined

TODO: bevisa den

Och nu nyttjar vi det i vårt huvudsakliga bevis.

> s4 :: Equals (Sub (Final V) (Initial V)) (Integ (Mul (Poly Zero Zero A_value) (Diff T)))
> s4 = x
>   where
>     x = transitivityInteg s3 y
>     -- y :: y :: forall c. Equals (Mul A c) (Mul (Poly Zero Zero A_value) c)
>     y = transitivityMul constantAcc

Nu kan vi beräkna integralen i rhs. Vi gör det generellt först. Notera att integralen *måste* vara map $t$.

> data Two

TODO: Introducera att $t_i=0$. Mycket av nedanstående följer ur det.

> integPoly :: Equals (Integ (Mul (Poly Zero c1 c0) (diff T))) 
>              (Poly (Div c1 Two) c0 Zero)
> integPoly = undefined

> s5 :: Equals (Sub (Final V) (Initial V)) 
>              (Poly (Div Zero Two) A_value Zero)
> s5 = transitivity' s4 integPoly

> divZero :: Equals (Div Zero x) Zero
> divZero = undefined

> transitivityPoly2 :: Equals c2 c2' -> Equals (Poly c2 c1 c0) (Poly c2' c1 c0)
> transitivityPoly2 = undefined

> s6 :: Equals (Sub (Final V) (Initial V)) 
>              (Poly Zero A_value Zero)
> s6 = transitivity' s5 (transitivityPoly2 divZero)

> data Add a b

Ekvivalens-karaktär på nedanstående

> subToAdd :: Equals (Sub a b) c -> Equals a (Add b c)
> subToAdd = undefined

> s7 :: Equals (Final V) (Add (Initial V) (Poly Zero A_value Zero))
> s7 = subToAdd s6

I sådana här sammanhang brukar man kalla den initiala hastigheten för $V_0$.

> data V_0

> initialVelocity :: Equals (Initial V) V_0
> initialVelocity = undefined

> transitivityAdd :: Equals a b -> Equals (Add a c) (Add b c)
> transitivityAdd = undefined

> s8 :: Equals (Final V) (Add V_0 (Poly Zero A_value Zero))
> s8 = transitivity' s7 (transitivityAdd initialVelocity)

Sammanfattningsvis har vi utifrån det allmäna sambandet

\begin{align}
  a(t) = \frac{dv(t)}{dt}
\end{align}

och givet att acceleration är konstant $a_värde$, $t_i=0$ samt att $v_0$ är den initiala hastigheten visat att

\begin{align}
  v_f = v_0 + a_värde * t
\end{align}

Där $v_f$ är en funktion av tiden. Man brukar skriva ovanstående som

\begin{align}
  v(t) = v_0 + a_värde * t
\end{align}
