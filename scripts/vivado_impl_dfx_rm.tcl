# TCL script used by the build to implement a Vivado DFX Reconfigurable Module.

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

# The virtual socket instance location in the design, e.g.
# boxlambda_soc_inst/vs0_inst. This script currently only supports one virtual
# socket. It should be extended so it can handle multiple virtual sockets.
getopt argv -vsCellInsts vsCellInsts ""

# The .dcp synthesis checkpoint of this reconfigurable module.
getopt argv -vsDcps vsDcps ""

# The routed static design checkpoint to link against.
getopt argv -staticDcp staticDcp ""

# Basename of the generated output files
getopt argv -outputBaseName outputBaseName ""

# FPGA part name, e.g. xc7a100ticsg324-1L
getopt argv -part part ""

puts "staticDcp: $staticDcp"
puts "outputBaseName: $outputBaseName"
puts "part: $part"

set vsCellInstList [regexp -all -inline {\S+} $vsCellInsts]
set vsDcpList [regexp -all -inline {\S+} $vsDcps]

foreach vsCellInst $vsCellInstList {
  puts "vsCellInst: $vsCellInst"
}

foreach vsDcp $vsDcpList {
  puts "vsDcp: $vsDcp"
}

foreach vsDcp $vsDcpList {
  if {![file exists [glob $vsDcp]]} {
    error "vsDcp not found: $vsDcp"
  }
}

if {![file exists $staticDcp]} {
  error "staticDcp not found: $staticDcp"
}

# Create an implementation project
create_project -force -part $part rm_impl_project

# The project consists of the route static design checkpoint and the synthesized virtual socket component (the
# reconfigurable module).
add_files $staticDcp

foreach vsCellInst $vsCellInstList vsDcp $vsDcpList {
  add_files [glob $vsDcp]
  set_property SCOPED_TO_CELLS $vsCellInst [get_files [glob $vsDcp]]

  #Link the reconfigurable module to the static design.
  link_design -mode default -reconfig_partitions $vsCellInst -part $part 
}

#Route the linked design
opt_design
place_design
route_design

# Generate two bitstream files: 
# - The full bitstream of the complete design, including the virtual socket component 
# - A partial bitstream of just the virtual socket component.
write_bitstream -force -bin_file $outputBaseName

# Convert the partial bitstream into a format that can be live-loaded by the
# DFX Controller.
source [get_property REPOSITORY [get_ipdefs *dfx_controller:1.0]]/xilinx/dfx_controller_v1_0/tcl/api.tcl 

set partialBinList [glob *_partial.bin]
puts "partialBinList $partialBinList"

foreach partial $partialBinList {
  dfx_controller_v1_0::format_bin_for_icap -bs 1 -i $partial
}

close_project

