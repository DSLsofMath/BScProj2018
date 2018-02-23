
Oskars instruktioner: använd enkla dollarteckan för att ha bättre kontroll över radbrytningar. Ha inga mellansslag. $Q$ och inte $ Q $ !

Haka på `--mathjax` för att `\frac` m.m. ska funka

Thermodynamics
==============

> module Thermodynamics
> (
> )
> where

Målet med det här kapitlet är att kunna beskriva kretsprocesser i Haskell. Men innan dess behöver teori och hjälpfunktioner skapas.

Ideala gaslagen är ett användbart samband.

$pV = nRT$

- $p$ är trycket
- $V$ är volymen
- $n$ är substansmängden
- $R$ är allmäna gaskonstanten, med numeriskt värde $8,3144621$
- $T$ är temperaturen

> r :: Double
> r = 8.3144621

---

Fundamentalt för kretsprocesser är termodynamikens första huvudsats. Den är

$dQ = dE_{int} + dW_{gas}$

eller om man integrer från "initialt" till "finalt" tillstånd.

$Q = \Delta E_{int} + W_{gas}$

$Q$ är den värme som tillförs gasen. $\Delta E_{int}$ är hur gasens interna energi förändras. $W_{gas}$ är det arbete som *gasen* utför på omgivningen.

De tre inblandade variablerna är av olika karaktär. $Q$ och $W_{gas}$ är *överföringsvariabler* som enbart finns vid en process, medan $\Delta E_{int}$ är en *tillståndsvariabel* som finns "hela tiden".

---

Finns det några slags uttryck för den interna energin och arbetet som kan kopplas till några andra variabler? Ja, det finns.

Formeln för intern energi är som följer. Är gasen enatomig gäller

$E_{int} = n \frac{3}{2} T$

och är den tvåatomig gäller

$E_{int} = n \frac{5}{2} T$

---

Vi har nu introducerat de egenskaper som man kan säga definerar ett system av en gas. Dessa är fixa hela tiden. Egenskaperna är antal atomer i gasen och hur stor substansmängd som finns av den.

> data Gas = Gas Int    -- Number of atoms
>                Double -- Amount of substance

Vi gör även en funktion som beräknar den interna energin i en gas utifrån dess nuvarande temperatur.

> -- Internal Energy From Temperature
> ieft :: Gas -> Double -> Double
> ieft (Gas na n) t = n*((fromIntegral na)*2+1)/2*t

Är inte temperatur, tryck och volym egenskaper hos en gas? Jo, det är de, men de ändras över tid. Så vi behandlar dem separat från de fixa variablerna.

---

För arbetet kan man härleda uttrycket utifrån bilden (som här saknas hehe).

\begin{align}
  dW &= F*dx \\
     &= (p*A)*dx \\
     &= p*(A*dx) \\
     &= p*dV
\end{align}

Det ger att

\begin{align}
  \int_i^f dW &= \int_i^f p*dV <-> \\
            W &= \int_i^f p*dV
\end{align}

Är trycket konstant kan det förenklas ytterliggare. Men vi kommer stöta på fall både då det är det, och då det inte är det, så vi väntar med att förenkla ytterligare.

---

En gas genomgår olika *kretsprocesser*. Den tar sig från ett tillstånd till ett annat i ett *PV-diagram*. Vi definerar ett sådant tillstånd i Haskell.

> data State = State Double -- Pressure
>                    Double -- Volume

För en viss gas kan nu temperaturen bestämmas om man vet vilket tillstånd man är i. Det ser man om ideala gaslagen skrivs om.

\begin{align}
  pV=nRT \iff T=\frac{pV}{nR}
\end{align}

> -- Temperature From State
> tfs :: Gas -> State -> Double
> tfs (Gas _ n) (State p v) = p*v/(n*r)

Vi gör även en funktion för att få intern energi direkt från ett tillstånd.

> -- Internal Energy From State
> iefs :: Gas -> State -> Double
> iefs gas = ieft' . tfs'
>   where
>     tfs' = tfs gas
>     ieft' = ieft gas

Varför välja just $p$ och $V$ som tillstånd? Det hade gått att välja exempelvis $p$ och $T$ som tillstånd men man brukar inte göra så.

---

Det finns fyra typer av processer som en gas genomgår. De har vardera ett karaktäristika.

| Typ     | Karaktäristiksa     |
|---------|---------------------|
| Isobar  | Konstant tryck      |
| Isokor  | Konstnat volym      |
| Isoterm | Konstant temperatur |
| Adiabat | Inget värmeutbyte   |

Vi ska nu göra funktioner som "genomför" en process på ett system. Gemensamt för alla fallen är att de startar i ett tillstånd och slutar i ett annat som man specifierar indirekt. De har också de två överföringsvariablerna tillförd värme och utfört arbete. Den inre energin kan redan beräknas från temperaturen, och därmed från tillstånden.

> data Energy = Energy Double -- Tillförd värme
>                      Double -- Utfört arbete

---

I en **isobar** process är trycket konstant. Det finala tillståndet specifieras därför av önskad final volym.

Vi beräknar vad arbetet ska bli. Från tidigare har vi att $W = \int_i^f p*dV$ och eftersom $p$ är konstant så blir $W= p*\int_i^f dV = p*\Delta V = p*(V_f-V_i)$

> isobar :: Gas -> State -> Double -> (State, Energy)
> isobar gas state@(State pi vi) vf = (newState, energy)
>   where
>     pf = pi                -- Constant pressure
>     newState = State pf vf -- The new final state
>     work = pf*(vf-vi)      -- Work performed by the gas
>     ei = iefs gas state    -- Initial internal energy
>     ef = iefs gas newState -- Final internal energy
>     ed = ef - ei           -- Difference in internal energy
>     q = ed + work          -- 1:a huvudsatsen
>     energy = Energy q work

---

I en **isokor** process är volymen konstant. Det finala tillståndet specifieras därför av önskat finalt tryck.

Arbetet som utförs är $0$ eftersom $dV=0$ hela tiden.

> isokor :: Gas -> State -> Double -> (State, Energy)
> isokor gas state@(State pi vi) pf = (newState, energy)
>   where
>     vf = vi -- Constant volume
>     newState = State pf vf
>     work = 0
>     ei = iefs gas state
>     ef = iefs gas newState
>     ed = ef - ei
>     q = ed -- No work, only internal energy difference
>     energy = Energy q work

---

I en **isoterm** process är temperaturen konstant. Det finala tillståndet specifieras därför av antingen ett önskat tryck eller en önskad volym. Temperaturen ändras inte, så den av tryck och volymn man inte specar måste anpassa sig. Eftersom temperaturen inte heller ändras så ändras inte den interna energin.

Vi behöver beräkna det arbete som utförs.

\begin{align}
  W &= \int_i^f p*dV \\
    &= \{ pV = nRT \iff p = \frac{nRT}{V} \} \\
    &= \int_i^f \frac{nRT}{V}*dV \\
    &= \{ nRT = const \} \\
    &= nRT * \int_{V_i}^{V_f} \frac{1}{V} * dV \\
    &= nRT *  [ln V]_{V_i}^{V_f} \\
    &= nRT * (ln V_f - ln V_i) \\
    &= nRT * ln \frac{V_f}{V_i}
\end{align}

> isoterm :: Gas -> State -> Either Double Double -> (State, Energy)
> isoterm gas@(Gas _ n) state@(State pi vi) pfORvf = (newState, energy)
>   where
>     ti = tfs gas state
>     nRT = n*r*ti -- nRT is constant
>     (pf,vf) = case pfORvf of
>                 Left  pf -> (pf, nRT/pf)
>                 Right vf -> (nRT/vf, vf)
>     newState = State pf vf
>     work = nRT*log (vf/vi)
>     q = work -- Only work, no internal energy change
>     energy = Energy q work










IDE: Ha isoterm, isobar m.m. som en syntax-träd!!!









-------------

Idela gaslagen: `pV = nRT`.

Inre energi: `E = n*(3/2)*R*T` för enatomiga och `E = n*(5/2)*R*T` för tvåatomiga. Har att göra med antal frihetsgrader.

Vägen som togs för inre energi spelar ingen roll.

----------

Värme och arbete är bara något som finns vid en process.

`dW = F*dx = p*A*dx = P*dV`

`W i->f = int i -> f p*dV`

Hur vi tar oss i till f spelar roll för arbetet.

------------

Isokor process. Då är arbetet `0` för `dV = 0` hela tiden.

Isobar process. Då är arbetet `p*(Vf-Vi)` När expanderar så gör positivt arbete.

Isoterm process. `pV=nRT` och `T` är konstant. `p = nRT/V` Gör man lite algebra får man att wg = nRT*ln(Vf/Vi). Fortfarande positivt arbete om expansion.

-------------

Termodynamikens 1:a huvudsats

`dQ = dE + dW` tillförd värme står för ökad inre energi och utfört arbete.

Fråga om det fastsvetsade locket och löst. Är arbetet för att lyfta locket mot gravitationskraften?

-----------

Specifikt värme är så mycket värme behöver tillföras för att öka med viss temperatur. Cv om volymen hålls konstant. Cp om trycket hålls konstant.

Cv är `3/2*R` för enatomig, eftersom vid konstant volym är `dW=0` så inget arbete. Allt går åt till den inre energi. 

Gör man lite beräkningar får man att `Cp=Cv+R`.

-------------

Adiabatisk process är termiskt isolerad från omgivningen. Inget värmeutbyte.

I `Q = deltaE + W` är `Q = 0` så om expanderar och gör arbete måste deltaE vara negativ, dvs minskar i temp, så kurvan ser ut som isoterm fast brantare.

För en isoterm gäller pV = konst

För en adiabat gäller pV^(Cp/Cv) 

gamma = Cp/Cv.

---------------

Sammanfattande tabell

| Värde       | Isokor                | Isobar                | Iosterm               | Adiabat               |
| ------------|-----------------------|-----------------------|-----------------------|-----------------------|
| $$ Q $$     | $$ n*C_v*(T_2-T_1) $$ | $$ n*C_p*(T_2-T_1) $$ | $$ nRT*ln(V_2/V_1) $$ | $$ 0 $$               |
| $$ E_int $$ | $$ n*C_v*(T_2-T_1) $$ | $$ n*C_v*(T_2-T_1) $$ | $$ 0 $$               | $$ n*C_v*(T_2-T_1) $$ |
| $$ W_gas $$ | $$ 0 $$               | $$ n*R*(T_2-T_1) $$   | $$ nRT*ln(V_2/V_1) $$ | $$ n*C_v*(T_1-T_2) $$ |

---------------

Verkningsgrad

$$ e = Wg/Q_t = summa all värme / summa positiv värme $$