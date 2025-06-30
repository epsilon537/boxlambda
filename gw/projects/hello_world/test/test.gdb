#This is a GDB script used in an automated test
#verifying OpenOCD based JTAG connectivity to a target
#running a 'Hello World' test build.

set architecture riscv:rv32

#Connect to openocd (which is connected to the target.
target extended-remote localhost:3333

#Continue to main()
b main
c
s
#Print the contents of a local variable.
p/x leds

#Disconnect from target.
monitor [target current] configure -event gdb-detach { shutdown }
