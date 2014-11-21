#=======================================================================
# UCB VLSI FLOW: Makefile for icc-par
#-----------------------------------------------------------------------

default: all

base_dir = $(abspath ../../..)
vlsi_dir = $(abspath ../..)
build_dir = $(abspath ..)

include $(base_dir)/Makefrag
include $(build_dir)/Makefrag
include $(build_dir)/dc-syn/Makefrag
include $(build_dir)/icc-par/Makefrag

#--------------------------------------------------------------------
# Default make target
#--------------------------------------------------------------------

.PHONY: prelude_icc init_design_icc place_opt_icc clock_opt_cts_icc clock_opt_psyn_icc clock_opt_route_icc route_icc route_opt_icc chip_finish_icc outputs_icc icc

all: icc

#--------------------------------------------------------------------
# Clean up
#--------------------------------------------------------------------

junk +=

clean:
	rm -rf build* current* $(junk) *~ \#*
