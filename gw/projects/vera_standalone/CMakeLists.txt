#gw_project_rules() function is defined in the parent CMakeLists.txt.
gw_project_rules(
    TOP_MODULE top
    PROJECT_NAME vera_standalone
    VERILATOR_CPP_FLAGS 
        "-I."
    VERILATOR_LD_FLAGS 
        -lncurses
)
