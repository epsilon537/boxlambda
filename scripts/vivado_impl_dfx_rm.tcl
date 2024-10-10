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
getopt argv -vsCellInst vsCellInst ""
getopt argv -vsDcp vsDcp ""
getopt argv -staticDcp staticDcp ""
getopt argv -outputBaseName outputBaseName ""

puts "vsCellInst: $vsCellInst"
puts "vsDcp: $vsDcp"
puts "staticDcp: $staticDcp"
puts "outputBaseName: $outputBaseName"

set part "xc7a100ticsg324-1L"

if {![file exists $vsDcp]} {
  error "vsDcp not found: $vsDcp"
}

if {![file exists $staticDcp]} {
  error "staticDcp not found: $staticDcp"
}

create_project -force -part $part rm_impl_project
add_files $staticDcp
add_files $vsDcp
set_property SCOPED_TO_CELLS $vsCellInst [get_files $vsDcp]
link_design -mode default -reconfig_partitions $vsCellInst -part $part 
#-top boxlambda_top
opt_design
place_design
route_design

write_bitstream -force -bin_file $outputBaseName
source [get_property REPOSITORY [get_ipdefs *dfx_controller:1.0]]/xilinx/dfx_controller_v1_0/tcl/api.tcl 
dfx_controller_v1_0::format_bin_for_icap -bs 1 -i [glob *partial.bin]

close_project
