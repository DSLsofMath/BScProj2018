LHS2TEX := lhs2TeX
LATEXMK := latexmk

%.tex : %.lhs
	$(LHS2TEX) --poly $< > $@

%.pdf : %.tex
	$(LATEXMK) -xelatex $<
