#We're using Vivado's project mode
open_project project.xpr

open_run synth_1

opt_design
place_design
route_design

write_bitstream -force -bin_file project
write_mem_info -force project

# Generate a timing and utilization reports and write to disk
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file imp_timing.rpt
report_utilization -quiet -file imp_util.rpt

close_project

