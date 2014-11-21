# create clock
create_clock clk -name clk -period ${CLOCK_PERIOD}
set_clock_uncertainty ${CLOCK_UNCERTAINTY} [get_clocks clk]

# set drive strength for inputs
set_driving_cell -lib_cell INVX1_RVT [all_inputs]
# set load capacitance of outputs
set_load -pin_load 0.004 [all_outputs]

set all_inputs_but_clk [remove_from_collection [all_inputs] [get_ports clk]]
set_input_delay ${INPUT_DELAY} -clock [get_clocks clk] $all_inputs_but_clk
set_output_delay ${OUTPUT_DELAY} -clock [get_clocks clk] [all_outputs]

# label libraries by threshold voltage
# set dbfile [lindex $TARGET_LIBRARY_FILES 0] 
# set len [string length $dbfile]
# set HVT_lib [string range $dbfile 0 [expr "$len - 4"]]
# echo HVT_lib = $HVT_lib
# 
# set dbfile [lindex $TARGET_LIBRARY_FILES 1] 
# set len [string length $dbfile]
# set RVT_lib [string range $dbfile 0 [expr "$len - 4"]]
# echo RVT_lib = $RVT_lib
# 
# set dbfile [lindex $TARGET_LIBRARY_FILES 2] 
# set len [string length $dbfile]
# set LVT_lib [string range $dbfile 0 [expr "$len - 4"]]
# echo LVT_lib = $LVT_lib
# 
# set_attribute [get_libs $LVT_lib] default_threshold_voltage_group LVt -type string
# set_attribute [get_libs $RVT_lib] default_threshold_voltage_group RVt -type string
# set_attribute [get_libs $HVT_lib] default_threshold_voltage_group HVt -type string

# alternative to set_leakage_optimization true
# another power optimization option is to specify max percentage of cells to 
# use from specified Vt libraries
#set_multi_vth_constraint -lvth_groups {LVT} -lvth_percentage 5.0 -type soft

# set_ungroup [get_designs FPU] false
# set_ungroup [get_designs IntToFP] false
# set_ungroup [get_designs FPUSFMAPipe] false
# set_ungroup [get_designs FPUDFMAPipe] false
# set_ungroup [get_designs vuVXU_Banked8_FU_imul] false
# set_ungroup [get_designs vuVXU_Banked8_FU_conv] false
set_ungroup [get_designs Frontend] false
set_ungroup [get_designs HellaCache] false

set_isolate_ports [all_outputs] -type buffer
set_isolate_ports [remove_from_collection [all_inputs] clk] -type buffer -force

# set_optimize_registers true -design IntToFP -clock clk -check_design -verbose -print_critical_loop -justification_effort high -minimum_period_only -sync_transform decompose
# set_optimize_registers true -design FPUSFMAPipe -clock clk -check_design -verbose -print_critical_loop -justification_effort high -minimum_period_only -sync_transform decompose
# set_optimize_registers true -design FPUDFMAPipe -clock clk -check_design -verbose -print_critical_loop -justification_effort high -minimum_period_only -sync_transform decompose
# set_optimize_registers true -design vuVXU_Banked8_FU_imul -clock clk -check_design -verbose -print_critical_loop -justification_effort high -minimum_period_only -sync_transform decompose
# set_optimize_registers true -design vuVXU_Banked8_FU_conv -clock clk -check_design -verbose -print_critical_loop -justification_effort high -minimum_period_only -sync_transform decompose

#set_optimize_registers true -design IntToFP -clock clk -check_design -verbose -print_critical_loop
#set_optimize_registers true -design FPUSFMAPipe -clock clk -check_design -verbose -print_critical_loop
#set_optimize_registers true -design FPUDFMAPipe -clock clk -check_design -verbose -print_critical_loop
#set_optimize_registers true -design vuVXU_Banked8_FU_imul -clock clk -check_design -verbose -print_critical_loop
#set_optimize_registers true -design vuVXU_Banked8_FU_conv -clock clk -check_design -verbose -print_critical_loop

#set_optimize_registers true -design superFMA -clock clk -check_design -verbose -print_critical_loop -minimum_period_only -sync_transform decompose
