#=======================================================================
# UCB VLSI FLOW: Makefile for dc-syn & fm-ver
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#
# This makefile will use Synopsys Design Compiler to synthesize
# your RTL into a gate-level verilog netlist.
# This makefile will use Synopsys Formality to verify
# generated gate-level verilog netlist against your RTL implementation.
#

default: all

base_dir = $(abspath ../../..)
vlsi_dir = $(abspath ../..)
build_dir = $(abspath ..)

include $(base_dir)/Makefrag
include $(build_dir)/Makefrag
include $(build_dir)/dc-syn/Makefrag

#--------------------------------------------------------------------
# Default make target
#--------------------------------------------------------------------

.PHONY: dc fm

all: dc

#--------------------------------------------------------------------
# Clean up
#--------------------------------------------------------------------

junk +=

clean:
	rm -rf build* current* $(junk) *~ \#*
