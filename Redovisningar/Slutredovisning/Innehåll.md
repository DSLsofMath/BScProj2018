## Krav

15-20 min presentation.
Ca 10 min diskussion.
MAX 3 deltagare.
Visuella hjälpmedel SKALL användas.



## Bedömning

### Innehåll

Gör bra urval av materia från rapporten och projektet. Presentera
anpassat till mottagare, situation,
specifika ämnesområdet.

### Struktur

Välstrukturerat innehåll, presentationen lätt att följa.  Tydligt
markerade inledning, avslutning. De olika delarna är sammanbundna till
en enhet. Övergång mellan olika talare och mellan avsnitt är "välplanerad" och leder inte till avbrott.

### Presentationsteknik

Var talare har god ögonkontakt o talar fritt.

### Visualisering

materialet som används är tydligt och innehåller inte för mkt
info. Lättöverskådligt. När material visas redogörs det tydligt och
begripligt.

### Tidsanpassning

Håller sig inom ramarna. Förhållandevis jämnt fördelat mellan
presenterare.

### Hantering av frågor

Svarar på bra sätt på relevanta frågor.



## Fackförel notes

Presentationen ska förmedla helheten men fokusera på de *viktiga* aspekterna i arbetet.

Presentationen ska *inte* vara en kortversion av rapporten.

Väcka intresse för ämnet;
Förmedla ett budskap;
*Diskutera* innehållet (metoder, resultat, slutsatser)



## Presentation

### Öppning

sammanfattning?


### Vad projektet är (Syfte)

Läromaterial. LYAP. DSL. Haskell.


### Varför (Bakgrund) - *Mer*

* DSLsofMath: En kurs given av Patrik Jansson som vi alla tagit vars syfte är att angripa de matematiska
domänerna från ett funktionellt programmeringsperspektiv. Att betrakta syntax
och typer och organisera dem i domänspecifika språk.

* Vi vill göra samma sak fast för ett annat domän. Hur väljer vi ett bra domän? Titta på tentastats!
TSS är sämst men där finns det redan ett BS projekt. Fysik är också dåligt!

* Tentastatistik: Dåliga stats, **VISA BILD/GRAF/TABELL**. Vad beror det på?
**LISTA MED SAKER SOM DET KAN BERO PÅ**, en av dem är någonting som vi kanske kan lösa mha DSL!

* Vi vill altså utveckla DSL för att få bukt med tentastatsen!

### Förklara alla viktiga koncept (Teori)

Superkort om Haskell. DSL. LHS

* Haskell: Ett funktionellt programmeringsspråk.

* DSL: Vad är ett domänspecifikt språk? 
  - SQL och LATEX är domänspecifika språk.
  - De har specifika domäner, databaser/textformatering
  - Dom är båda väldigt bra på att hantera just det domänet
  - Båda är turingkompletta men du skulle inte vilja skriva en kompilator i dem
  - Du kan hantera båda domänen i Java men i slutändan skulle du bara implementera
    språken igen.
  - Ett DSL är ett språk som hanterar *ett* specifikt domän väldigt väl. Den abstraherar
  bort komplexitet och ger dig ett interface som är lätt att använda och vars syntaxt är
  specifikt modellerad för just det domänet. 

* LHS: 
  - **BILD PÅ KOD MED KOMMENTARER** **BILD PÅ KOD I LHS MED KOMMENTARERNA SOM TEXT**
  - Lyfter fram dokumentationen som en lika viktig del i programmet som själva koden.
  - Passar bra för denna typ av lärotext där syftet är att förklara fysik mha kod. 

* Syntaxträd???

* Lärandeteorier???

### Genomförande

Åke är intressant. Exakta urvalsprocessen mindre så. Vilka områden som
valdes kan va najs, och varför de valdes. Nåt om implementation? Hur hemsidan gjordes?

Diskutera om metoden var bra. Varför vi gjorde som vi gjorde.

* Fysik är ett enormt domän & kort projekt -> gör ett urval (Mekanik)
* Möte med Åke Fäldt, drog två slutsatser
  - Egna modeller är felaktiga -> Gör det rigoröst
  - Dåliga på matematisk analys -> Ett helt kapitel om analys
* Ögnade igenom kursboken 
* Experimentering med att implementera DSL
* Identifierade grundläggande och komposita områden
  - ...
* Implementerade väl avgränsade områden separat
  - ...
* Publicerade på hemsida


### Resultat

* Kapitel 
  - Matematisk analys
  - Vektorer
  - Dimensioner
  - Partikelmekanik
* Demonstrera sidan live!
* Kodexempel!
  - Komplicerat matematisk uttryck
  - Visa som syntaxträd?
  - Kanonifiera?
  - Visa små syntaktiska "bevis"?
* Kommentarer
  - Åke Fäldt: 
    - Jättebra sätt att strukturera upp dom matematiska modellerna
    - Villig att peka på det i fyskkursen som någonting att titta på
    - ...
  - Roger:
    - ...
  - DNS?
  - Testgruppen?

 * Diskutera ur pedagogisk synpunkt.
  - ...


### Slutsatser

redogör och diskutera dem


### Avslut

Känns typ samma som slutsats? Bara ett par ord för att tacka kanske?




## Saker att ha med (brainstorm):

Baserat på DSLsofMath.

Vad DSLsofMath handlar om.

Hypotesen: Att DSL kan hjälpa brygga gapet mellan matte o programmering.

Hitta kurs att applicera DSL på. TSS o Regler har typ sämst
tentastatistik, men har redan gjorts. Ville hellre gö nåt nytt. Fysik
har ganska dålig tentastatistik med, och innehåller mycket tillämpad
matematik - perfekt!

Kolla på fysik. Varför dåligt? Vi tror D studenter finner den svår
eller ointressant. DSL kan hjälpa både att göra lärande lättare, och
så att det känns mer relevant (pga. funktionell programmering).



