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

# Launch Implementation
launch_runs impl_1 -to_step write_bitstream
wait_on_run impl_1 ; #wait for implementation to complete before continuing
# Generate a timing and utilization reports and write to disk
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file $outputDir/imp_timing.rpt
report_utilization -quiet -file $outputDir/imp_util.rpt

close_project
