
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

Variablerna är i det generella fallet en funktion av tid. Och som det kommer visa sig, polynomfunktioner av tid.

> data Poly name a2 a1 a0

I `Poly` är `name` det som används för att hänvisa till variabeln. Sedan är funktionen på formen

\begin{align}
  var(t) = a_2*t^2 + a_1*t + a_0
\end{align}

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

