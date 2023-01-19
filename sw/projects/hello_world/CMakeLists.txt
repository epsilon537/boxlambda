add_executable(hello_world
    ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello/hello.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

link_internal_create_mem(hello_world)
