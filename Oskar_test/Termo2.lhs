
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

Fundamentalt för kretsprocesser är termodynamikens första huvudsats. Den är

$dQ = dE_{int} + dW_{gas}$

eller om man integrer från "initialt" till "finalt" tillstånd.

$Q = \Delta E_{int} + W_{gas}$

De tre inblandade variablerna är av olika karaktär. $Q$ och $W_{gas}$ är *överföringsvariabler* som enbart finns vid en process, medan $\Delta E_{int}$ är en *tillståndsvariabel* som finns "hela tiden".

$Q$ är den värme som tillförs gasen. $\Delta E_{int}$ är hur gasens interna energi förändras. $W_{gas}$ är det arbete som *gasen* utför på omgivningen.

Finns det några slags uttryck för den interna energin och arbetet som kan kopplas till några andra variabler? Ja, det finns.

Formeln för intern energi är som följer. Är gasen enatomig gäller

$E_{int} = n \frac{3}{2} T$

och är den tvåatomig gäller

$E_{int} = n \frac{5}{2} T$

Energin hos en fix mängd gas är med andra ord enbart proportionellt mot temperaturen. En konsekvens blir då att vilken "väg" en process tar inte spelar någon roll för den inre energin så länge start- och sluttemperatur är de samma.

Vi skapar lite Haskell-kod för detta

< iefs :: Int -> Double -> Double -> Double
< iefs na n t = n * (na*2 + 1) / 2 * t

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

Olika typer av processer
------------------------

Det finns fyra typer av processer som en gas genomgår. De har vardera ett karaktäristika.

| Typ     | Karaktäristiksa     |
|---------|---------------------|
| Isobar  | Konstant tryck      |
| Isokor  | Konstnat volym      |
| Isoterm | Konstant temperatur |
| Adiabat | Inget värmeutbyte   |

En process går från ett tillstånd till ett annat. Vad definerarer att tillstånd? Tryck och volym.

**Isobar**






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