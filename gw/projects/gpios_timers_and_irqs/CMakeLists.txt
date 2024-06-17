#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  set(top_module boxlambda_top)
else()
  set(top_module sim_main)
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME gpios_timers_and_irqs
    MEM_FILE_TARGET gpios_timers_and_irqs
    VERILATOR_CPP_FLAGS
        "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    VERILATOR_LD_FLAGS
        -lncurses
)

#Add testcase.
add_test(NAME gpios_timers_and_irqs_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

