if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module ibex_soc)
else()
    set(top_module sim_main)
endif()


gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME picolibc_test
    MEM_FILE_TARGET picolibc_test
    MEM_FILE_OUT spram.mem
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    VERILATOR_LD_FLAGS 
        -lncurses
)

add_test(NAME picolibc_test_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
