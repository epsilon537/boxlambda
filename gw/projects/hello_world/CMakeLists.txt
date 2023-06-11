#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ibex_soc
    PROJECT_NAME hello_world
    MEM_FILE_TARGET hello_world
    VERILATOR_CPP_FLAGS "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
    VERILATOR_LD_FLAGS "-lncurses"
)

#Add testcase.
add_test(NAME hello_world_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
