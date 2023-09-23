#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE reset_ctrl 
    COMPONENT_NAME reset_ctrl 
)

add_test(NAME reset_ctrl_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/reset_ctrl_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)