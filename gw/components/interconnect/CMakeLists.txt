#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE interconnect_ooc
    COMPONENT_NAME interconnect
)

add_test(NAME wb_arbiter_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/wb_arbiter_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)


