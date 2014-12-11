DOCUMENT := poster.tex
PARTS := example
LATEX := pdflatex
BIBTEX := bibtex
INKSCAPE := inkscape

TEXSRC := $(wildcard *.tex)
SVGSRC := $(wildcard img/*.svg)
SVGPDF := $(SVGSRC:.svg=.pdf)

RERUN := "(undefined references|Rerun to get (cross-references|the bars|point totals) right|Table widths have changed. Rerun LaTeX.|Linenumber reference failed)"
RERUNBIB := "No file.*\.bbl|Citation.*undefined"

all: $(DOCUMENT:.tex=.pdf)

latexmk:
	@latexmk -pvc -pdf $(DOCUMENT)

purge:
	-rm -f *.{aux,bbl,blg,dvi,fdb_latexmk,fls,log,out,thm,toc}

clean: purge
	-rm -f $(DOCUMENT:.tex=.pdf)
	-rm -f $(SVGPDF)

show: poster.pdf
	open -a Preview poster.pdf

unlock.pdf: $(addsuffix .tex, $(PARTS))

poster.pdf: poster.tex $(TEXSRC) $(SVGPDF)
	$(LATEX) $<
	@egrep -q $(RERUNBIB) $*.log && $(BIBTEX) $* && $(LATEX) $<; true
	@egrep -q $(RERUN) $*.log && $(LATEX) $<; true
	@egrep -q $(RERUN) $*.log && $(LATEX) $<; true
	-@egrep -qi undefined $*.log && printf '\n=== Undefined ====\n\n' && egrep -i undefined $*.log && echo

%.pdf: %.svg
	$(INKSCAPE) -f $< -D -A $@

.SUFFIXES: .tex .pdf .aux .dvi .log .bbl .blg .thm .out
