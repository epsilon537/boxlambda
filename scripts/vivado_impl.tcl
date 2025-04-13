# TCL script used by the build system to implement a (non-DFX) Vivado project.

#BoxLambda uses Vivado's project mode. All projects are called project.xpr.
open_project project.xpr

#Open the synthesis checkpoint
open_run synth_1

#Route the project
opt_design
place_design
phys_opt_design
route_design
phys_opt_design

write_checkpoint -force project_impl.dcp

#Create the bitstream file
write_bitstream -force project

#Generate the .mmi file to be used for post-synthesis memory updates.
#-quiet to silently ignore errors. Not all project can produce .mmi files.
write_mem_info -quiet -force project

# Generate a timing and utilization reports and write to disk
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file imp_timing.rpt

# Run timing report and capture output
set timing_report [report_timing_summary -return_string]

# Check if the timing report detected any timing violations
set timing_constraints_not_met [regexp -all -inline {Timing constraints are not met} $timing_report]

if {$timing_constraints_not_met != ""} {
    puts "ERROR: Timing violations detected! See imp_timing.rpt"
    exit 1
} else {
    puts "PASS: Timing met."
}

report_utilization -quiet -file imp_util.rpt

close_project

