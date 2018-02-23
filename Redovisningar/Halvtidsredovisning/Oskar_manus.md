
Oskars manus utifrån presentationsbilderna.

# Projektplanering

Tack Björn. Jag ska nu prata lite om projektplaneringen.

Det första steget i arbetet har varit att identifiera de problemområden som finns i Fysik för ingenjörer. Det har vi bland annat gjort genom att prata med kursens föreläsare och själva reflekterat över vad vi tyckt var svårt när vi läste kursen.

Det andra steget har varit inläsning av fysik, så att vi kan hitta intressanta områden att behandla. Det har varit inläsning av domänspecifika språk för att bättra för kunskaper om dem. Det har till sist varit inläsning av liknande områden för att få inspiration.

Det tredje steget har varit att skriva själva läromaterialet, parallellt med implementation av domänspecika språk, och det är på detta steg vi är nu.

Som bilden är tänkt att visa så är orange brödtext och grön programkod sammanvävda. Så i processen att skapa läromaterialet måste dessa också vara sammanvävda.

# Nuvarande status - 1

Nuvarande status. Kollar vi på det gantt-schema vi ställt upp bör vi se var vi ska befinna oss, och det stämmer ganska väl med verkligheten. Vi håller just nu på att implementera domänspecifika språk och skriva läromaterial. Däremot är den inledande inläsningen och identifikation av problemområden i princip klara. Mer inläsning kommer vi göra löpande vid behov.

# Nuvarande status - 2

Dom områden som är avslutade är enheter och vektorer. Till dom har både skapa domänspecifika språk och skrivit läromaterial. Just nu så arbetar vi med differentialkalkyl och fysikaliska kroppar.

# Smakprov: enheter

För att ge ett smakprov på vad resultatet blivit av vårt arbete tänkte jag vissa lite från ett syntax-träd som vi använder till differentialkalkyl.

Trädet för ett uttryck består av konstruktorer för addition av två uttryck, multiplikation av två uttryck samt en konstant.

Till exempel för att skriva 7 gånger 3 plus 10, så skriver man Const 7 :*: (Const 3 :+: Const 10).

Det illustreras av det svartvita trädet. Multiplikationen är roten. Det som gångras är 7 och ett deluttryck, med den lokala roten addition. Och additionen är mellan 3 och 10.

För att evaluera ett sådant träd delar man upp i olika fall, ett för varje konstruktor. Till exempel är addition de två delträden evaluderas, och deras resultat adderat.

Att göra ett sånt här träd kallas deep embedding, eftersom man fångar upp mycket detaljrikedom från det man modellerar och kan därför manipulera datan bra.

# Egna erfarenheter

Vi upptäckte att de saker i Fysik för ingenjörer som varit svåra inte är fysiken i sig. Istället har det varit den tillämpade matematik som ingår som varit det svåra.

Det har inneburit att de domänspecifika språk vi skapat mer handlat om matematik än fysik.

Dessutom är fysik-uppgifter inget annat än räkneexempel i matematik, så det är svårt att modellera fysik i sig.

# Avslutning

För att sammanfatta vad kandidatarbetet handlar om, så är syftet att väcka intresse för fysik hos datastudenter. Och det tänker vi göra genom att skapa ett läromaterial som handlar om fysik sammanvävt med att domänspecifika språk för fysik skapas.

Och eftersom vi gillar Haskell och vi gillar fysik, så vi tror detta kan bli ett skoj grej att kombinera dem!

Med det sagt, tack för oss! Har ni några frågor?


