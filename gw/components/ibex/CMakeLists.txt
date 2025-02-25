#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE ibex_top
    COMPONENT_NAME ibex
)

add_test(NAME ibex_no_prefetch_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/ibex_single_prefetch_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
