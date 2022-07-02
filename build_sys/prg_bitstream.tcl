#Simple script hacked together to download a bitstream to an IP connected target (i.e. you have hw_server running somewhere on your network).
#Pretty much wired my own current setup. I haven't made any attempt yet to system-ize and/or product-ize it.
#Customize as needed for your own setup.

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

getopt argv -bitstream bitstream ""
getopt argv -ip ip ""

puts "bitstream: $bitstream"
puts "ip: $ip"

open_hw_manager
connect_hw_server -url $ip:3121 -allow_non_jtag
current_hw_target [get_hw_targets */xilinx_tcf/Digilent/210319B0C4AEA]
set_property PARAM.FREQUENCY 15000000 [get_hw_targets */xilinx_tcf/Digilent/210319B0C4AEA]
open_hw_target
current_hw_device [get_hw_devices xc7a35t_0]
refresh_hw_device -update_hw_probes false [lindex [get_hw_devices xc7a35t_0] 0]
set_property PROBES.FILE {} [get_hw_devices xc7a35t_0]
set_property FULL_PROBES.FILE {} [get_hw_devices xc7a35t_0]
set_property PROGRAM.FILE $bitstream [get_hw_devices xc7a35t_0]
program_hw_devices [get_hw_devices xc7a35t_0]
refresh_hw_device [lindex [get_hw_devices xc7a35t_0] 0]
