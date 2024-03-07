#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE  syncpls
    COMPONENT_NAME cdc_cells
)

#A CocoTB-based test program testing the PicoRV_DMA core.
add_test(NAME syncpls_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/syncpls_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
