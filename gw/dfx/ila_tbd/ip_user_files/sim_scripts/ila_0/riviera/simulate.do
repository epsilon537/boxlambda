transcript off
onbreak {quit -force}
onerror {quit -force}
transcript on

asim +access +r +m+ila_0  -L xpm -L xil_defaultlib -L unisims_ver -L unimacro_ver -L secureip -O5 xil_defaultlib.ila_0 xil_defaultlib.glbl

do {ila_0.udo}

run 1000ns

endsim

quit -force
