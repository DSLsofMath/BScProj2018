
Oskars kompileringsinsturktioner

Skriv `.lhs`-filer med bird-style `> ` för kod och `< ` för "kod" som inte ska kompileras.

Kompilera med kommandot `pandoc FILNAMN.lhs -f markdown+lhs -t html -o FILNAMN.html -s --mathjax`