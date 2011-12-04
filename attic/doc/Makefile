DIAGRAMS = ints_and_floats.tex numbers.tex

manual.pdf: manual.tex $(DIAGRAMS)
	pdflatex manual.tex

$(DIAGRAMS): %.tex: %.dot
	dot -Txdot $< | dot2tex-2.6 --autosize --usepdflatex --codeonly > $@

clean:
	rm -f manual.pdf $(DIAGRAMS)
