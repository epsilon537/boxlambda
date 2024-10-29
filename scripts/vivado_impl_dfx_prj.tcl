# TCL script used by the build system to implement a Vivado DFX (Dynamic
# function exchange - a.k.a. partial FPGA reconfiguration) project.

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

#Command line arguments accepted by the script:

# The virtual socket instance location in the design, e.g.
# boxlambda_soc_inst/vs0_inst. This script currently only supports one virtual
# socket. It should be extended so it can handle multiple virtual sockets.
getopt argv -vsCellInst vsCellInst ""

# The .dcp synthesis checkpoint of the virtual socket default component.
getopt argv -vsDefaultDcp vsDefaultDcp ""

# The virtual socket partition block constraints file, specifying where in the
# FPGA floorplan the reconfigurable partition should be located.
getopt argv -vsPblockConstr vsPblockConstr ""

puts "vsCellInst: $vsCellInst"
puts "vsDefaultDcp: $vsDefaultDcp"
puts "vsPblockConstr: $vsPblockConstr"

#We're using Vivado's project mode
open_project project.xpr

#Open the synthesis checkpoint of the static design
open_run synth_1

if {![file exists $vsDefaultDcp]} {
  error "vsDefaultDcp not found: $vsDefaultDcp"
}

if {![file exists $vsPblockConstr]} {
  error "vsPblockConstr not found: $vsPblockConstr"
}

#Indicate that this project has a reconfigurable partition at location
#$vsCellInst
set_property HD.RECONFIGURABLE TRUE [get_cells $vsCellInst]
update_design -quiet -cell $vsCellInst -black_box
#Associate the default component's synthesis checkpoint with is configurable
#partition.
read_checkpoint -cell $vsCellInst $vsDefaultDcp

puts "Reading Pblock constraints..."
read_xdc -unmanaged -no_add $vsPblockConstr

puts "Routing...."
opt_design
place_design
route_design

# Generate two bitstream files: 
# - The full bitstream of the complete design, including the virtual socket 
#   default component 
# - A partial bitstream of just the virtual socket default component.
write_bitstream -force -bin_file dfx_project

# Convert the partial bitstream into a format that can be live-loaded by the
# DFX Controller.
source [get_property REPOSITORY [get_ipdefs *dfx_controller:1.0]]/xilinx/dfx_controller_v1_0/tcl/api.tcl
dfx_controller_v1_0::format_bin_for_icap -bs 1 -i [glob *_partial.bin]

# Generate a static route checkpoint with an empty virtual socket.
update_design -cell $vsCellInst -black_box
lock_design -level routing
write_checkpoint -force dfx_project.static_route.dcp

# Generate a timing and utilization reports and write to disk
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose \
-max_paths 10 -input_pins -file imp_timing.rpt
report_utilization -quiet -file imp_util.rpt

close_project

