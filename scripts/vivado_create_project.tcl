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
getopt argv -sources sources ""
getopt argv -constraints constraints ""
getopt argv -mem_files mem_files ""
getopt argv -outputDir outputDir ""
getopt argv -top top ""

puts "project: $project"
puts "part: $part"

#sources is a generated TCL script adding HDL sources to the Vivado project
puts "sources: $sources"

#constraints is a generated TCL script that adds a constraints file to the Vivado project
puts "constraints: $constraints"

#mem_files is a generated TCL script that adds memory files to the Vivado project
puts "mem_files: $mem_files"

puts "outputDir: $outputDir"

#top specifies the top module name
puts "top: $top"

#We're using Vivado's project mode
create_project $project -part $part -force
source $sources

#Only source the mem_file script if it's passed in and actually exists.
if {($mem_files != "") && ([file exists $mem_files] == 1)} {
    puts "sourcing mem files list." 
    puts $mem_files
    source $mem_files
} 

#Only source the constraints script if it's passed in and actually exists.
if {($constraints != "") && ([file exists $constraints] == 1)} {
    puts "sourcing constraints file list." 
    puts $constraints
    source $constraints
} 

set_property source_mgmt_mode None [current_project]

update_compile_order -fileset sources_1

set_property top $top [current_fileset]

#Suppress INFO messages
set_msg_config -severity INFO -suppress

close_project
