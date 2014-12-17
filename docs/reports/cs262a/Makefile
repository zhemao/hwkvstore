ROOT_DIR := ../..

SVGSRC := system-kvstore.svg kvstore.svg frontend.svg hasher.svg \
	dma-tx.svg dma-rx.svg NIC.svg rocket-pipeline.svg
TEXDIR := . ../common
BIBDIR := $(TEXDIR)
DOCUMENT := paper.tex

include $(ROOT_DIR)/Makefrag

all: $(DOCUMENT_PDF)

show: paper.pdf
	open -a Preview paper.pdf
