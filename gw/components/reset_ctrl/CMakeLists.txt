#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE reset_ctrl 
    COMPONENT_NAME reset_ctrl 
)

configure_file(${CMAKE_CURRENT_LIST_DIR}/test/Makefile cocotb.mk @ONLY)
file(CREATE_LINK ${CMAKE_CURRENT_LIST_DIR}/test/reset_ctrl_test.py ${CMAKE_CURRENT_BINARY_DIR}/reset_ctrl_test.py SYMBOLIC)

#Add testcase. This one's a script, not just the Verilator Vmodel executable.
add_test(NAME reset_ctrl_test
    COMMAND make -f cocotb.mk
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
