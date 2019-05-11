.PHONY: all
all: notes.pdf intro-slides.pdf

notes.pdf: notes.tex references.bib
	latexmk -lualatex -latexoption=-shell-escape $(PREVIEW_CONTINUOUSLY) notes.tex

intro-slides.pdf: intro-slides.tex
	TEXINPUTS=$(TEXINPUTS):./beamer-trek latexmk -lualatex intro-slides.tex

.PHONY: watch
watch: PREVIEW_CONTINUOUSLY='-pvc'
watch: notes.pdf intro-slides.pdf

.PHONY: clean
clean:
	rm -rf *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.toc _minted-notes *.nav *.snm

.PHONY: mrclean
mrclean: clean
	rm -f notes.pdf intro-slides.pdf
