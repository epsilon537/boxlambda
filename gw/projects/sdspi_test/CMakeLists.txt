#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module sdspi_test_soc)
else()
    set(top_module sim_main)    
endif()

set(sw_project sdspi_test)

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME sdspi_test
    MEM_FILE_TARGET ${sw_project}
    MEM_FILE_OUT spram.mem
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang -I${PROJECT_SOURCE_DIR}/sub/sdspi/bench/cpp"
    VERILATOR_LD_FLAGS 
        -lncurses
)

#Add testcase.
add_test(NAME sdspi_test_test
    COMMAND ./Vmodel -s ${CMAKE_CURRENT_LIST_DIR}/test/sdcard.img
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

#This test requires a longer timeout.
set_tests_properties(sdspi_test_test PROPERTIES TIMEOUT 400) 