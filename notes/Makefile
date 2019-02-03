SOURCE_DOCS := $(wildcard *.md)
EXPORTED_DOCS=\
 $(SOURCE_DOCS:.md=.pdf)

PANDOC=pandoc
PANDOC_OPTIONS=--from=markdown
PANDOC_PDF_OPTIONS=\
 --pdf-engine=lualatex \
 -H preamble.tex \
 -V fontsize=12pt \
 -V documentclass:book \
 -V papersize:a4paper \
 --top-level-division=chapter \
 --filter pandoc-citeproc
RM=rm

%.pdf : %.md
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_PDF_OPTIONS) -o $@ $<

.PHONY: all clean

all : $(EXPORTED_DOCS)

clean:
	- $(RM) $(EXPORTED_DOCS)
