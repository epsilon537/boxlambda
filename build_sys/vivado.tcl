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

getopt argv -project project ""
getopt argv -part part "unknown"
getopt argv -cmd cmd ""
getopt argv -sources sources ""
getopt argv -constraints constraints ""
getopt argv -mem_files mem_files ""
getopt argv -outputDir outputDir ""

puts "project: $project"
puts "part: $part"
puts "cmd: $cmd"
puts "sources: $sources"
puts "constraints: $constraints"
puts "mem_files: $mem_files"
puts "outputDir: $outputDir"

create_project $project -part $part -force
source $sources
if {($mem_files != "") && ([file exists $mem_files] == 1)} {
    source $mem_files
}

if {($constraints != "") && ([file exists $constraints] == 1)} {
    source $constraints
}

update_compile_order -fileset sources_1

if {($cmd == "synth") || ($cmd == "impl")} {
    # Launch Synthesis
    launch_runs synth_1
    wait_on_run synth_1
    open_run synth_1 -name netlist_1
    # Generate a timing and utilization report and write to disk
    report_timing_summary -quiet -delay_type max -report_unconstrained -check_timing_verbose \
	-max_paths 10 -input_pins -file $outputDir/syn_timing.rpt
    report_utilization -quiet -file $outputDir/syn_util.rpt
}

if {$cmd == "impl"} {
    # Launch Implementation
    launch_runs impl_1 -to_step write_bitstream
    wait_on_run impl_1 
    # Generate a timing and utilization reports and write to disk
    # comment out the open_run for batch mode
    open_run impl_1
    report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
    -max_paths 10 -input_pins -file $outputDir/imp_timing.rpt
    report_utilization -quiet -file $outputDir/imp_util.rpt
}

close_project
