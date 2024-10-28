# TCL script used by the build system to implement a (non-DFX) Vivado project.

#BoxLambda uses Vivado's project mode. All projects are called project.xpr.
open_project project.xpr

#Open the synthesis checkpoint
open_run synth_1

#Route the project
opt_design
place_design
route_design

#Create the bitstream file
write_bitstream -force -bin_file project

#Generate the .mmi file to be used for post-synthesis memory updates.
#-quiet to silently ignore errors. Not all project can produce .mmi files.
write_mem_info -quiet -force project

# Generate a timing and utilization reports and write to disk
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file imp_timing.rpt
report_utilization -quiet -file imp_util.rpt

close_project

