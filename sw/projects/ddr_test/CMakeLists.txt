add_executable(ddr_test
	EXCLUDE_FROM_ALL
	ddr_test.c
    memtest.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
	../../../sub/litex/litex/soc/software/liblitedram/sdram.c
)

target_include_directories(ddr_test
		PRIVATE
    		../../../sub/litex/litex/soc/software/liblitedram
			../../../sub/litex/litex/soc/software/include
			../../../sub/litex/litex/soc/software
    		../../../sub/litex/litex/soc/cores/cpu/ibex
)

target_compile_definitions(ddr_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
		-DMAIN_RAM_BASE=0x40000000UL
		-DMAIN_RAM_BASE2=0x50000000UL
		-DCSR_BASE=0x10030000
		-DSDRAM_TEST_DISABLE
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
	target_include_directories(ddr_test
		PRIVATE
			../../../gw/components/litedram/sim/sw/include
	)

	target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=32768)

else() #fpga:
	target_include_directories(ddr_test
		PRIVATE
			../../../gw/components/litedram/arty/sw/include
	)

	target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

link_internal_create_mem_file(ddr_test)
