add_library(sdram)

target_sources(sdram
    PRIVATE
    ../../../sub/litex/litex/soc/software/liblitedram/sdram.c
    ../../../sub/litex/litex/soc/software/liblitedram/accessors.c
)

target_compile_definitions(sdram
    PUBLIC
        -DCONFIG_CPU_NOP="nop"
        -DMAIN_RAM_BASE=0x40000000UL
        -DMAIN_RAM_BASE2=0x50000000UL
        -DCSR_BASE=0x10030000
        -DSDRAM_TEST_DISABLE
)

target_compile_options(sdram
	PRIVATE
		-O2
)

target_include_directories(sdram
    PUBLIC
        ${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/software/liblitedram
		${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/software/include
		${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/software
    	${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/cores/cpu/ibex
)

#The memory controller SW interface is different between simulation and FPGA targets.
if(CMAKE_BUILD_TYPE STREQUAL "sim")
	target_include_directories(sdram
		PUBLIC
            ${PROJECT_SOURCE_DIR}/gw/components/litedram/sim/sw/include
	)
else() #fpga:
	target_include_directories(sdram
		PUBLIC
            ${PROJECT_SOURCE_DIR}/gw/components/litedram/arty/sw/include
	)
endif()
