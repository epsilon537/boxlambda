#FPGA and simulation builds have different top-levels.
set(top_module boxlambda_top)

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME rtcc_test
    MEM_FILE_TARGET cmem_to_flash_vector #rtcc_test
    VERILATOR_CPP_FLAGS
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang -I${PROJECT_SOURCE_DIR}/sub/wbi2c/bench/cpp"
    VERILATOR_LD_FLAGS
        -lncurses
)

