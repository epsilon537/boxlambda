add_executable(hello_world
	EXCLUDE_FROM_ALL
    ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello/hello.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

target_compile_options(hello_world
	PRIVATE -g)

link_internal_create_mem_file(hello_world)
