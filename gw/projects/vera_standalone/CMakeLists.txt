if(CMAKE_BUILD_TYPE STREQUAL "sim")
    #This build has an SDL dependency. VERA's VGA output is rendered onto an SDL window.
    find_package(SDL2 REQUIRED)
endif()

#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE vera_top
    PROJECT_NAME vera_standalone
    VERILATOR_CPP_FLAGS 
        "-I${PROJECT_SOURCE_DIR}/sub/vera_wishbone/misc -I${SDL2_INCLUDE_DIRS}"
    VERILATOR_LD_FLAGS 
        "-lncurses ${SDL2_LIBRARIES}"
)
