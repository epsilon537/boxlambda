
# Create Project

create_project -force -name litedram -part 
set_msg_config -id {Common 17-55} -new_severity {Warning}

# Add project commands


# Add Sources

read_verilog {/home/epsilon/work/boxlambda/gw/components/litedram/fpga_25mhz/rtl/litedram.v}

# Add EDIFs


# Add IPs


# Add constraints

read_xdc litedram.xdc
set_property PROCESSING_ORDER EARLY [get_files litedram.xdc]

# Add pre-synthesis commands


# Synthesis

synth_design -directive default -top litedram -part 

# Synthesis report

report_timing_summary -file litedram_timing_synth.rpt
report_utilization -hierarchical -file litedram_utilization_hierarchical_synth.rpt
report_utilization -file litedram_utilization_synth.rpt
write_checkpoint -force litedram_synth.dcp

# Add pre-optimize commands


# Optimize design

opt_design -directive default

# Add pre-placement commands


# Placement

place_design -directive default

# Placement report

report_utilization -hierarchical -file litedram_utilization_hierarchical_place.rpt
report_utilization -file litedram_utilization_place.rpt
report_io -file litedram_io.rpt
report_control_sets -verbose -file litedram_control_sets.rpt
report_clock_utilization -file litedram_clock_utilization.rpt
write_checkpoint -force litedram_place.dcp

# Add pre-routing commands


# Routing

route_design -directive default
phys_opt_design -directive default
write_checkpoint -force litedram_route.dcp

# Routing report

report_timing_summary -no_header -no_detailed_paths
report_route_status -file litedram_route_status.rpt
report_drc -file litedram_drc.rpt
report_timing_summary -datasheet -max_paths 10 -file litedram_timing.rpt
report_power -file litedram_power.rpt

# Bitstream generation

write_bitstream -force litedram.bit 

# End

quit