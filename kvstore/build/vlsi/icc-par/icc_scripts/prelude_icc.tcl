##########################################################################################
# Version: D-2010.03-SP2 (July 6, 2010)
# Copyright (C) 2007-2010 Synopsys, Inc. All rights reserved.
##########################################################################################
##########################################################################################
## init_design_icc.tcl : initial scripts that reads the design, applies constraints and
##                   generates a zero interconnect timing report
##########################################################################################

source icc_setup.tcl

########################################################################################
# Design Creation
########################################################################################

if { $ICC_INIT_DESIGN_INPUT == "MW" } {

########################################################################################
# MW CEL as the format between DCT and ICC
########################################################################################

 open_mw_cel $ICC_INPUT_CEL -library $MW_DESIGN_LIBRARY

  if {$DFT && $ICC_DP_DFT_FLOW && !$ICC_SKIP_IN_BLOCK_IMPLEMENTATION} {
    if {[file exists [which $ICC_IN_FULL_CHIP_SCANDEF_FILE]]} {
    	read_def $ICC_IN_FULL_CHIP_SCANDEF_FILE
    } else {
    	echo "SCRIPT-Error: $ICC_DP_DFT_FLOW is set to true but SCANDEF file $ICC_IN_FULL_CHIP_SCANDEF_FILE is not found. Please investigate it"	
    }
  }

}


if {$ICC_INIT_DESIGN_INPUT != "MW" } {
  if { ![file exists [which $MW_DESIGN_LIBRARY/lib]] } {
     if { [file exists [which $MW_REFERENCE_CONTROL_FILE]]} {
       create_mw_lib \
            -tech $TECH_FILE \
            -bus_naming_style {[%d]} \
            -reference_control_file $MW_REFERENCE_CONTROL_FILE \
            $MW_DESIGN_LIBRARY
     } else {
       create_mw_lib \
            -tech $TECH_FILE \
            -bus_naming_style {[%d]} \
            -mw_reference_library $MW_REFERENCE_LIB_DIRS \
            $MW_DESIGN_LIBRARY
       }
  }
}


if {$ICC_INIT_DESIGN_INPUT == "DDC" } {

########################################################################################
# DDC as the format between DCT and ICC
########################################################################################

  open_mw_lib $MW_DESIGN_LIBRARY
  suppress_message "UID-3"      ;# avoid local link library messages
  import_designs $ICC_IN_DDC_FILE -format ddc -top $DESIGN_NAME -cel $DESIGN_NAME
  unsuppress_message "UID-3"

  if {$DFT && $ICC_DP_DFT_FLOW && !$ICC_SKIP_IN_BLOCK_IMPLEMENTATION} {
    if {[file exists [which $ICC_IN_FULL_CHIP_SCANDEF_FILE]]} {
    	remove_scan_def
    	read_def $ICC_IN_FULL_CHIP_SCANDEF_FILE
    } else {
    	echo "SCRIPT-Error: $ICC_DP_DFT_FLOW is set to true but SCANDEF file $ICC_IN_FULL_CHIP_SCANDEF_FILE is not found. Please investigate it"	
    }
  }

}


if {$ICC_INIT_DESIGN_INPUT == "VERILOG" } {

########################################################################################
# Ascii as the format between DCT and ICC
########################################################################################

 open_mw_lib $MW_DESIGN_LIBRARY

 ## add -dirty_netlist in case there are mismatches between the VERILOG netlist and the FRAM view of the cells
 read_verilog -top $DESIGN_NAME $ICC_IN_VERILOG_NETLIST_FILE
 uniquify_fp_mw_cel
 current_design $DESIGN_NAME
      read_sdc $ICC_IN_SDC_FILE
  if {$DFT && $ICC_DP_DFT_FLOW && !$ICC_SKIP_IN_BLOCK_IMPLEMENTATION} {
    if {[file exists [which $ICC_IN_FULL_CHIP_SCANDEF_FILE]]} {
    	read_def $ICC_IN_FULL_CHIP_SCANDEF_FILE
    } else {
    	echo "SCRIPT-Error: $ICC_DP_DFT_FLOW is set to true but SCANDEF file $ICC_IN_FULL_CHIP_SCANDEF_FILE is not found. Please investigate it"	
    }
  }
}

report_area

foreach_in_collection sram [get_cells -hierarchical -filter "mask_layout_type==macro"] {
  puts "SRAM INFO"
  puts [get_attribute $sram full_name]
  puts [get_attribute $sram ref_name]
  puts [get_attribute $sram width]
  puts [get_attribute $sram height]
}

exit
