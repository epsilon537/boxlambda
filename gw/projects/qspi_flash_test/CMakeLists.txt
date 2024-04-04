#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module top)
else()
    set(top_module sim_main)    
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME qspi_flash_test
    MEM_FILE_TARGET qspi_flash_test
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang/ -I${PROJECT_SOURCE_DIR}/sub/qspiflash/bench/cpp/"
    VERILATOR_LD_FLAGS 
        -lncurses
)

#Add testcase.
add_test(NAME qspi_flash_test_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
