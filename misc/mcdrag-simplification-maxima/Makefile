mcdrag-formulas.pdf : mcdrag-formulas.tex mcdrag.mac
	latexmk -f -interaction=nonstopmode -r ./latexmkrc -lualatex mcdrag-formulas.tex < /dev/null

clean:
	latexmk -C -silent
	rm -rf mcdrag-formulas.tex_mac mcdrag-formulas.tex_mac.log mcdrag-formulas.tex_mxp

all : mcdrag-formulas.pdf
