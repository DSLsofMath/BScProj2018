default: Main.pdf

%.pdf: %.tex
	latexmk -pdf $<

Main.pdf:: \
	include/Diskussion.tex \
	include/Introduktion.tex \
	include/Metod.tex \
	include/Resultat.tex \
	include/Slutsatser.tex \
	include/Teori.tex \
	include/backmatter/Utdrag.tex \
	include/frontmatter/Abstract.tex \
	include/frontmatter/Acknowledgements.tex \
	include/frontmatter/Ordlista.tex \
	include/frontmatter/Sammanfattning.tex \
	include/frontmatter/Titlepage.tex \
	include/settings/Settings.tex

clean:
	latexmk -C Main
