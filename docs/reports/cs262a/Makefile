ROOT_DIR := ../..

SVGSRC := kvstore.svg frontend.svg hasher.svg
TEXDIR := . ../common
BIBDIR := $(TEXDIR)
DOCUMENT := paper.tex

include $(ROOT_DIR)/Makefrag

all: $(DOCUMENT_PDF)

show: paper.pdf
	open -a Preview paper.pdf
