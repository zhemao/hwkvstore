ROOT_DIR := ..

include $(ROOT_DIR)/Makefrag

DOCUMENT := poster.tex
DOCUMENT_PDF := $(DOCUMENT:.tex=.pdf)

TEXSRC := $(wildcard *.tex)

all: $(DOCUMENT_PDF)

latexmk:
	@latexmk -pvc -pdf $(DOCUMENT)

purge:
	-rm -f *.{aux,bbl,blg,dvi,fdb_latexmk,fls,log,out,thm,toc}

clean: purge
	-rm -f $(DOCUMENT_PDF)
	-rm -f $(SVGPDF)

show: $(DOCUMENT)
	open -a Preview poster.pdf

unlock.pdf: $(addsuffix .tex, $(PARTS))

$(DOCUMENT_PDF): $(DOCUMENT) $(TEXSRC) $(SVGPDF)
	$(LATEX) $<

.SUFFIXES: .tex .pdf .aux .dvi .log .bbl .blg .thm .out
