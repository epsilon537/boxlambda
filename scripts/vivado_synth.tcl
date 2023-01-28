#TCL script to interface the build system to vivado.
proc getopt {_argv name {_var ""} {default ""}} {
     upvar 1 $_argv argv $_var var
     set pos [lsearch -regexp $argv ^$name]
     if {$pos>=0} {
         set to $pos
         if {$_var ne ""} {
             set var [lindex $argv [incr to]]
         }
         set argv [lreplace $argv $pos $to]
         return 1
     } else {
         if {[llength [info level 0]] == 5} {set var $default}
         return 0
     }
}

#Command line arguments accepted by the script
getopt argv -project project ""
getopt argv -outputDir outputDir ""

puts "project: $project"

puts "outputDir: $outputDir"

#We're using Vivado's project mode
open_project $project

#Reset is needed before launch.
reset_run synth_1

# Launch Synthesis
launch_runs synth_1
wait_on_run synth_1 ; #wait for synthesis to complete before continuing
open_run synth_1 -name netlist_1
# Generate a timing and utilization report and write to disk
report_timing_summary -quiet -delay_type max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file $outputDir/syn_timing.rpt
report_utilization -quiet -file $outputDir/syn_util.rpt

close_project
