LHS2TEX := lhs2TeX
LATEXMK := latexmk

%.tex : %.lhs %.format
	$(LHS2TEX) --verb $< > $@

%.pdf : %.tex
	$(LATEXMK) -xelatex $<

clean :
	rm -f *.pdf *.xdv *.aux *.fls *.tex *.fdb_latexmk *.log *.ptb
