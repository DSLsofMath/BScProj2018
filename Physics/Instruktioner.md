
# LHS

- Brödtext-formatering med markdown
  - Dock inte `#`. Istället så använd `===` och `---` för rubrik och underrubrik.
  - Skriv `$F = a * m$` för inline-matte och `\begin{align}` för flera rader. Bråk funkar ej i inline.
  - För bild som text flyter runt: `![FIGURTEXT + EV. COPYRIGHT INFO](URL){.float-img-right}` eller `left` i den.
  - För block-bild: `![FIGURTEXT + EV. COPYRIGHT INFO](URL)`
- Kod-formatering
  - Skriv med `>` för kod och `<` för "kod" som inte ska komileras.
  - Eller använd `\begin{code}` och `\begin{spec}` men de kan inte blandas.
  - Koden ska vara max 80 tecken bred.

# Pandoc

Komilera med ` pandoc FILNAMN.lhs -f markdown+lhs -t html -o FILNAMN.html -s --mathjax`.

# Python

???