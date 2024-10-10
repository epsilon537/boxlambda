#TCL script to interface the build system to vivado.
#This script implements (routes) a synthesized design.
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
getopt argv -vsCellInst vsCellInst ""
getopt argv -vsDefaultDcp vsDefaultDcp ""
getopt argv -vsPblockConstr vsPblockConstr ""

puts "project: $project"
puts "vsCellInst: $vsCellInst"
puts "vsDefaultDcp: $vsDefaultDcp"
puts "vsPblockConstr: $vsPblockConstr"

#We're using Vivado's project mode
open_project project.xpr

open_run synth_1

puts "DFX routing default configuration."

if {![file exists $vsDefaultDcp]} {
  error "vsDefaultDcp not found: $vsDefaultDcp"
}

if {![file exists $vsPblockConstr]} {
  error "vsPblockConstr not found: $vsPblockConstr"
}

set_property HD.RECONFIGURABLE TRUE [get_cells $vsCellInst]
update_design -quiet -cell $vsCellInst -black_box
read_checkpoint -cell $vsCellInst $vsDefaultDcp

puts "Reading Pblock constraints..."
read_xdc -unmanaged -no_add $vsPblockConstr

puts "Routing...."
opt_design
place_design
route_design

write_bitstream -force -bin_file $project
write_mem_info -force $project

source [get_property REPOSITORY [get_ipdefs *dfx_controller:1.0]]/xilinx/dfx_controller_v1_0/tcl/api.tcl
dfx_controller_v1_0::format_bin_for_icap -bs 1 -i [glob *_partial.bin]

update_design -cell $vsCellInst -black_box
lock_design -level routing
write_checkpoint -force $project.static_route.dcp

# Generate a timing and utilization reports and write to disk
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file imp_timing.rpt
report_utilization -quiet -file imp_util.rpt

close_project

