notes.pdf : notes.tex references.bib
	latexmk -lualatex notes.tex

clean:
	rm -rf *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.toc

mrclean: clean
	rm -f notes.pdf

all : notes.pdf
