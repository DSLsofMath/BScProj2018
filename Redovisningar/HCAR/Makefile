default: Main.pdf

%.pdf: %.tex
	latexmk -pdf $<

clean:
	latexmk -C Main
