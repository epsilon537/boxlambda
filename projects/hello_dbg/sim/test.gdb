#Connect to openocd (which is connected to the target.
target remote localhost:3333

#Dump the address of the uart0 registers.
p/x uart0.registers

#Disconnect from target.
monitor [target current] configure -event gdb-detach { shutdown }