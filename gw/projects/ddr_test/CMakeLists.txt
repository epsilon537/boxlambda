#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module top)
else()
    set(top_module sim_main)
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME ddr_test
    MEM_FILE_TARGET ddr_test
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    VERILATOR_LD_FLAGS 
        -lncurses
)

#Add testcase
add_test(NAME ddr_test_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

#This test requires a longer timeout.
set_tests_properties(ddr_test_test PROPERTIES TIMEOUT 200) 