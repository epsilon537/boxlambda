#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module ym2149_test_soc)
else()
    set(top_module sim_main)    
endif()

set(sw_project ym2149_test)

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME ym2149_test
    MEM_FILE_TARGET ${sw_project}
    MEM_FILE_OUT spram.mem
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    VERILATOR_LD_FLAGS 
        -lncurses
)
