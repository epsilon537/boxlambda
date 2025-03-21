add_library(sdram)

target_sources(sdram
    PRIVATE
    ../../../sub/litex/litex/soc/software/liblitedram/sdram.c
    ../../../sub/litex/litex/soc/software/liblitedram/accessors.c
)

target_compile_definitions(sdram
    PUBLIC
        -DCONFIG_CPU_NOP="nop"
        -DMAIN_RAM_BASE=0x20000000UL
        -DCSR_BASE=0x10030000
        -DSDRAM_TEST_DISABLE
)

target_compile_options(sdram
 PRIVATE
  -O2 -g -ffunction-sections
)

target_include_directories(sdram
    PUBLIC
      ${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/software/liblitedram
      ${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/software/include
      ${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/software
      ${PROJECT_SOURCE_DIR}/sub/litex/litex/soc/cores/cpu/ibex
)

#The memory controller SW interface is different between simulation and FPGA targets.
target_include_directories(sdram
    PUBLIC
      ${CMAKE_BINARY_DIR}/codegen/litedram/sw/include
)
