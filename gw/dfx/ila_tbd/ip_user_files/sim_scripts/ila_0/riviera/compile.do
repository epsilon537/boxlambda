transcript off
onbreak {quit -force}
onerror {quit -force}
transcript on

vlib work
vmap -link {/home/epsilon/work/boxlambda/gw/dfx/ila/managed_ip_project/managed_ip_project.cache/compile_simlib/riviera}
vlib riviera/xpm
vlib riviera/xil_defaultlib

vlog -work xpm  -incr "+incdir+../../../../ila_0/hdl/verilog" -l xpm -l xil_defaultlib \
"/tools/Xilinx/Vivado/2023.1/data/ip/xpm/xpm_cdc/hdl/xpm_cdc.sv" \
"/tools/Xilinx/Vivado/2023.1/data/ip/xpm/xpm_memory/hdl/xpm_memory.sv" \

vcom -work xpm -93  -incr \
"/tools/Xilinx/Vivado/2023.1/data/ip/xpm/xpm_VCOMP.vhd" \

vlog -work xil_defaultlib  -incr -v2k5 "+incdir+../../../../ila_0/hdl/verilog" -l xpm -l xil_defaultlib \
"../../../../ila_0/sim/ila_0.v" \

vlog -work xil_defaultlib \
"glbl.v"

