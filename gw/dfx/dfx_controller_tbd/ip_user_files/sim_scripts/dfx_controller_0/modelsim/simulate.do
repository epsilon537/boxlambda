onbreak {quit -f}
onerror {quit -f}

vsim -voptargs="+acc"  -L xpm -L xbip_utils_v3_0_10 -L axi_utils_v2_0_6 -L xbip_pipe_v3_0_6 -L lib_cdc_v1_0_2 -L lib_pkg_v1_0_2 -L lib_srl_fifo_v1_0_2 -L c_reg_fd_v12_0_6 -L xbip_dsp48_wrapper_v3_0_4 -L xbip_dsp48_addsub_v3_0_6 -L xbip_addsub_v3_0_6 -L c_addsub_v12_0_15 -L c_gate_bit_v12_0_6 -L xbip_counter_v3_0_6 -L c_counter_binary_v12_0_16 -L dfx_controller_v1_0_4 -L xil_defaultlib -L unisims_ver -L unimacro_ver -L secureip -lib xil_defaultlib xil_defaultlib.dfx_controller_0 xil_defaultlib.glbl

set NumericStdNoWarnings 1
set StdArithNoWarnings 1

do {wave.do}

view wave
view structure
view signals

do {dfx_controller_0.udo}

run 1000ns

quit -force
