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
getopt argv -part part "unknown"
getopt argv -cmd cmd ""
getopt argv -sources sources ""
getopt argv -constraints constraints ""
getopt argv -mem_files mem_files ""
getopt argv -outputDir outputDir ""
getopt argv -top top ""

puts "project: $project"
puts "part: $part"

#synth or impl, or something else like 'dryrun' if you just want to generate the Vivado project without actually kicking off synthesis or implementation.
puts "cmd: $cmd"

#sources is a generated TCL script adding HDL sources to the Vivado project
puts "sources: $sources"

#constraints is a generated TCL script add a constraints file to the Vivado project
puts "constraints: $constraints"

#mem_files is a generated TCL script to add memory files to the Vivado project
puts "mem_files: $mem_files"

puts "outputDir: $outputDir"

#top specifies the top module name
puts "top: $top"

#We're using Vivado's project mode
create_project $project -part $part -force
source $sources

#Only source the mem_file script if it's passed in and actually exists.
if {($mem_files != "") && ([file exists $mem_files] == 1)} {
    source $mem_files
}

#Only source the constraints script if it's passed in and actually exists.
if {($constraints != "") && ([file exists $constraints] == 1)} {
    source $constraints
}

update_compile_order -fileset sources_1

set_property top $top [current_fileset]

#The synthesis step
if {($cmd == "synth") || ($cmd == "impl")} {
    # Launch Synthesis
    launch_runs synth_1
    wait_on_run synth_1 ; #wait for synthesis to complete before continuing
    open_run synth_1 -name netlist_1
    # Generate a timing and utilization report and write to disk
    report_timing_summary -quiet -delay_type max -report_unconstrained -check_timing_verbose \
	-max_paths 10 -input_pins -file $outputDir/syn_timing.rpt
    report_utilization -quiet -file $outputDir/syn_util.rpt
}

#The implementation step
if {$cmd == "impl"} {
    # Launch Implementation
    launch_runs impl_1 -to_step write_bitstream
    wait_on_run impl_1 ; #wait for implementation to complete before continuing
    # Generate a timing and utilization reports and write to disk
    open_run impl_1
    report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
    -max_paths 10 -input_pins -file $outputDir/imp_timing.rpt
    report_utilization -quiet -file $outputDir/imp_util.rpt
}

close_project
