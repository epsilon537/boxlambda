#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  set(top_module boxlambda_top)

else()
  set(top_module sim_main)
endif()

set(sw_project fatfs_test)

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME fatfs_test
    MEM_FILE_TARGET ${sw_project}
    VERILATOR_CPP_FLAGS
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang -I${PROJECT_SOURCE_DIR}/sub/sdspi/bench/cpp"
    VERILATOR_LD_FLAGS
        -lncurses
)

#Add testcase.
add_test(NAME fatfs_test_test
    COMMAND ./Vmodel -s ${CMAKE_CURRENT_LIST_DIR}/test/sdcard.img
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
