
# Innehåll till halvtidsredovisningen

## Projektets mål

- Göra fysik roligare och relevantare (=> enklare) för datateknologer (effektmål)
- Slutprodukt i form av handledning där fysik är mixat med domänspecika språk för fysik (projektmål)

## Projektplanering

- Hitta problemområden
- Inläsning av diverse saker
- Skriva handledning paralellt med implementation
  - I prioritetsordningen

## Nuvarande status

- ???

## Egna erfarenheter

- Det svåra i fysikkursen har till stor delvarit den tillämade matematiken

# "Manus" kopplat till presentationsbilder

- **Introduktion**
  - Kandidatarbetet "Matematikens domänspecika språk"
  - Avgrening från kursen med samma namn. Kandidatarbetet och kursen i mångt och mycket lika men olika innehåll
    - Presenterar matematik från ett annat perspektiv
      - Uppmärksammar syntax och typer hos matematik.
      - Använder Haskell för att skapa domänspecfika språk
        - Ett domänspecifikt språk, är ett språk gjort för en specifk domän. Så att saker i domänen kan uttryckas enklare än i ett generellt språk.
      - Ex: derivata i Haskell... i slutet   
    - Orsaken till detta är att TSS och regler har dålig tentastatistik, och examinatorerna tror detta orsakas av ovana vid den matematik som används.
  - Ett tidigare kandidatarbete år 2016
    - Läromaterial för TSS
    - Komplement till TSS-kursen. 
    - Brödtext och programmeringsövningar om olika koncept i TSS
- **Projektets mål**
  - Göra fysik intressantare (=> enklare) för datateknologer
    - Genom att presentera fysik i samband med domänspecika språk och funktionell programmering i Haskell
    - Som datateknologer förhoppningsvis tycker är både roligt och enklare än fysik
  - Fokuserar på kursen "Fysik för ingenjörer", som läses av data i 2:an. 
    - Börja med problemområden
    - Sedan mekanik, sedan termodynamik och vågrörelselära i mån av tid
  - Genom att göra ett läromaterial för fysik i stil med LYAH
    - Tutorial för Haskell
    - Lättsam stil, exempel i brödtext
  - Handledningen ska vara brödtext om fysik varvat med skapandet av domänspecika språk till fysik
  - Presentera fysikaliska problem och modellera dem i våra DSL
    - Kommer visa senare hur detta ser ut just nu
- **Projektplanering**
  - Hitta problemområden
    - Fråga föreläsaren, DNS och vad vi själva tyckt
  - Inläsning av diverse saker
    - Fysik
      - Kursboken *University Physics*
      - Föreläsningsanteckningar
    - DSL:er
      - Majoriten har läst DSLsofMath. Kolla den kursboken
    - Liknande arbeten
      - SICM
  - Sedan skriva brödtext och parallellt implementera DSLs
    - Resultat ihopvävt, så när ska göra också ihopvävt
    - Detta är den stora delen, även om vi inte sagt så mycket om den. Kan dela upp efter olika områden vid behov
- **Nuvarande status**
  - Vilka områden som är avklarade
  - Vad vi tänker göra härnäst
  - Visa något DSL som vi har gjort
- **Egna erfarenheter**
  - Tillämpad mateatik
    - Det svåra har varit att tillämpa matematik, inte själva fysikaliska koncepten
    - T.ex. Differentialkalkyl
- **Avslutning**
  - Sammanfattningsvis så ska det bli en handledning för fysik mixat med DSL:er. Väcka intresse för fysik hos datateknologer
  - Vi tror det kan bli en skoj grej, för vi gillar Haskell och vi gillar fysik
  - Frågor från publiken
  
# Enkelt Haskell exempel
  
    -- Syntax-träd. Ett DSL för funktionsuttryck
    data Fun = Fun + Fun
             | Fun * Fun
             | Konst Double
             | X
    --
    -- Matte-op "derviera" går att se som en funktion
    -- som tar en funktion som indata och ger en
    -- ny funktion som utdata
    derivera :: Fun -> Fun
    derivera (f1 * f2) = (f1' * f2) + (f1 * f2')
      where
        f1' = derivera f1
        f2' = derivera f2
