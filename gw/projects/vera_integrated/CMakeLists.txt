#FPGA and simulation builds have different top-levels.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    set(top_module vera_test_soc)
else()
    set(top_module sim_main)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "sim")
    #This build has an SDL dependency. VERA's VGA output is rendered onto an SDL window.
    find_package(SDL2 REQUIRED)
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE ${top_module}
    PROJECT_NAME vera_integrated
    MEM_FILE_TARGET vera_test
    MEM_FILE_OUT spram.mem
    VERILATOR_CPP_FLAGS 
        "-g -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang -I${SDL2_INCLUDE_DIRS}"
    VERILATOR_LD_FLAGS 
        "-g -lncurses ${SDL2_LIBRARIES}"
)

#Add testcase
#add_test(NAME vera_integrated_test
#    COMMAND ./Vmodel
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#)

#This test requires a longer timeout.
#set_tests_properties(vera_integrated_test PROPERTIES TIMEOUT 200) 