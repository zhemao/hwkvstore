source dc_setup.tcl

#################################################################################
# Setup for Formality verification
#################################################################################

set_svf ${RESULTS_DIR}/${DCRM_SVF_OUTPUT_FILE}

#################################################################################
# Read in the RTL Design
#################################################################################

define_design_lib WORK -path ./WORK
analyze -format verilog ${RTL_SOURCE_FILES}
elaborate ${DESIGN_NAME}
#analyze_datapath_extraction > ${REPORTS_DIR}/${DESIGN_NAME}.datapath.rpt
remove_unconnected_ports [find -hier cell "*"]
remove_unconnected_ports -blast_buses [find -hier cell "*"]
write -hierarchy -format ddc -output ${RESULTS_DIR}/${DCRM_ELABORATED_DESIGN_DDC_OUTPUT_FILE}
link

#################################################################################
# Apply Logical Design Constraints
#################################################################################

source -echo ${DCRM_CONSTRAINTS_INPUT_FILE}

#################################################################################
# Create Default Path Groups
#
# Separating these paths can help improve optimization.
# Remove these path group settings if user path groups have already been defined.
#################################################################################

set ports_clock_root [filter_collection [get_attribute [get_clocks] sources] object_class==port]
group_path -name REGOUT -to [all_outputs]
group_path -name REGIN -from [remove_from_collection [all_inputs] $ports_clock_root]
group_path -name FEEDTHROUGH -from [remove_from_collection [all_inputs] $ports_clock_root] -to [all_outputs]

# multi-vt flow
set_leakage_optimization true

##################################################################################
# Specify ignored layers for routing to improve correlation
# Use the same ignored layers that will be used during place and route
##################################################################################
if { ${MIN_ROUTING_LAYER} != ""} {
  set_ignored_layers -min_routing_layer ${MIN_ROUTING_LAYER}
}
if { ${MAX_ROUTING_LAYER} != ""} {
  set_ignored_layers -max_routing_layer ${MAX_ROUTING_LAYER}
}
#report_ignored_layers

#################################################################################
# Apply Additional Optimization Constraints
#################################################################################
# Prevent assignment statements in the Verilog netlist.
set_fix_multiple_port_nets -all -buffer_constants

#################################################################################
# Compile the Design
#################################################################################

check_design

compile_ultra -gate_clock -no_autoungroup

#################################################################################
# Write Out Final Design and Reports
#
#        .ddc:   Recommended binary format used for subsequent Design Compiler sessions
#        .v  :   Verilog netlist for ASCII flow (Formality, PrimeTime, VCS)
#        .sdf:   SDF backannotated topographical mode timing for PrimeTime
#        .sdc:   SDC constraints for ASCII flow
#
#################################################################################

change_names -rules verilog -hierarchy
set_svf -off
write -format ddc -hierarchy -output ${RESULTS_DIR}/${DCRM_FINAL_DDC_OUTPUT_FILE}
write -f verilog -hierarchy -output ${RESULTS_DIR}/${DCRM_FINAL_VERILOG_OUTPUT_FILE}
write_sdc -nosplit ${RESULTS_DIR}/${DCRM_FINAL_SDC_OUTPUT_FILE}

source find_regs.tcl
find_regs ${STRIP_PATH}

#################################################################################
# Generate Final Reports
#################################################################################

report_qor > ${REPORTS_DIR}/${DCRM_FINAL_QOR_REPORT}
report_timing -input_pins -capacitance -transition_time -nets -significant_digits 4 -nosplit -nworst 10 -max_paths 10 > ${REPORTS_DIR}/${DESIGN_NAME}.mapped.timing.rpt
report_area -hierarchy -physical -nosplit > ${REPORTS_DIR}/${DCRM_FINAL_AREA_REPORT}
report_area -hierarchy -nosplit > ${REPORTS_DIR}/${DCRM_FINAL_AREA_REPORT}
report_power -nosplit -hier > ${REPORTS_DIR}/${DCRM_FINAL_POWER_REPORT}
report_clock_gating -nosplit > ${REPORTS_DIR}/${DCRM_FINAL_CLOCK_GATING_REPORT}
report_reference -nosplit -hierarchy > ${REPORTS_DIR}/${DESIGN_NAME}.mapped.reference.rpt
report_resources -nosplit -hierarchy > ${REPORTS_DIR}/${DESIGN_NAME}.mapped.resources.rpt
report_constraint -all_violator > ${REPORTS_DIR}/${DESIGN_NAME}.mapped.constraint.rpt
report_cell -nosplit >  ${REPORTS_DIR}/${DESIGN_NAME}.mapped.cell.rpt

exit
