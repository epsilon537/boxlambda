add_executable(vera_test
	EXCLUDE_FROM_ALL
	vera_test.c
	vera_hal.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
	../../../sub/litex/litex/soc/software/liblitedram/sdram.c
)

target_include_directories(vera_test
		PRIVATE
			../../../sub/vera-module/misc
    		../../../sub/litex/litex/soc/software/liblitedram
			../../../sub/litex/litex/soc/software/include
			../../../sub/litex/litex/soc/software
    		../../../sub/litex/litex/soc/cores/cpu/ibex
)

target_compile_definitions(vera_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
		-DMAIN_RAM_BASE=0x40000000UL
		-DMAIN_RAM_BASE2=0x50000000UL
		-DCSR_BASE=0x10030000
		-DSDRAM_TEST_DISABLE
		-DVERA_BASE=0x10100000
)

#The memory controller SW interface is different between simulation and FPGA targets.
if(CMAKE_BUILD_TYPE STREQUAL "sim")
	target_include_directories(vera_test
		PRIVATE
			../../../gw/components/litedram/sim/sw/include
	)

else() #fpga:
	target_include_directories(vera_test
		PRIVATE
			../../../gw/components/litedram/arty/sw/include
	)

endif()

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(vera_test)
