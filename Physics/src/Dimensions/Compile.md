
Oskars kompileringsinsturktioner

Skriv `.lhs`-filer med bird-style `> ` för kod och `< ` för "kod" som inte ska kompileras.

Kompilera med kommandot `pandoc FILNAMN.lhs -f markdown+lhs -t html -o FILNAMN.html -s --mathjax`

Använd enkla dollarteckan för att ha "inline". Ha inga mellansslag. `$Q$` och inte `$ Q $` ! Använd `\begin{align}` och `\end{align}` för flera rader.

Haka på `--mathjax` för att `\frac` m.m. ska funka