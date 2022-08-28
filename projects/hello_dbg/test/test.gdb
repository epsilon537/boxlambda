#This is a GDB script used in an automated test
#verifying OpenOCD based JTAG connectivity to a target
#running a 'Hello World' test build.

#Connect to openocd (which is connected to the target.
target remote localhost:3333

#Dump the address of the uart0 registers.
#(i.e. this script assumes that the SW running on target has
#uart0 instantiated.
p/x uart0.registers

#Disconnect from target.
monitor [target current] configure -event gdb-detach { shutdown }