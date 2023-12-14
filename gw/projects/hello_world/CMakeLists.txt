#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module top)
else()
    set(top_module sim_main)    
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME hello_world
    MEM_FILE_TARGET hello_world
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    VERILATOR_LD_FLAGS 
        -lncurses
)

#Add testcase.
add_test(NAME hello_world_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

#Add testcase. This one's a script, not just the Verilator Vmodel executable.
add_test(NAME hello_dbg_test
    COMMAND ${CMAKE_CURRENT_LIST_DIR}/test/test.sh ${PROJECT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
