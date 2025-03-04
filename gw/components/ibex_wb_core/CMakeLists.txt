#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE ibex_wb_core_ooc
    COMPONENT_NAME ibex_wb_core
)

add_test(NAME core2wb_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/core2wb_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
