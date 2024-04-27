#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  set(top_module boxlambda_top)
else()
  set(top_module sim_main)
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME spiflash_test
    MEM_FILE_TARGET cmem_to_flash_vector #spiflash_test_ram
    VERILATOR_CPP_FLAGS
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang/ -I${PROJECT_SOURCE_DIR}/sub/qspiflash/bench/cpp/"
    VERILATOR_LD_FLAGS
        -lncurses
)

#Add the SW image to be loaded from flash as a dependency.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  add_dependencies(spiflash_test_bit_sw spiflash_test)
else()
  add_dependencies(spiflash_test_sim_sw spiflash_test)
endif()

#Add testcase.
add_test(NAME spiflash_test_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

