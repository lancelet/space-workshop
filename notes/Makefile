notes.pdf: notes.tex references.bib
	latexmk -lualatex -latexoption=-shell-escape $(PREVIEW_CONTINUOUSLY) notes.tex

.PHONY: watch
watch: PREVIEW_CONTINUOUSLY='-pvc'
watch: notes.pdf

.PHONY: clean
clean:
	rm -rf *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.toc

.PHONY: mrclean
mrclean: clean
	rm -f notes.pdf

.PHONY: all
all: notes.pdf
