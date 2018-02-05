LHS2TEX := lhs2TeX
LATEXMK := latexmk

%.tex : %.lhs %.format
	$(LHS2TEX) --poly $< > $@

%.pdf : %.tex
	$(LATEXMK) -xelatex $<

clean :
	rm *.pdf *.xdv *.aux *.fls *.tex *.fdb_latexmk *.log *.ptb
