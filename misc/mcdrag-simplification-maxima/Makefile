mcdrag-formulas.pdf : mcdrag-formulas.tex mcdrag.mac
	latexmk -e '$$pdflatex=q/lualatex --shell-escape %O %S/' -pdf mcdrag-formulas.tex

clean:
	rm -rf *.aux *.fdb_latexmk *.fls *.log *.mac.log *.mxp *.pdf
	rm -rf mcdrag-formulas.mac
