set(top_module audio_dac_test)

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME audio_dac_test
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    VERILATOR_LD_FLAGS 
        -lncurses        
)
