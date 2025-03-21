#Code generate the ibex core.
execute_process(
    COMMAND
      ${PROJECT_SOURCE_DIR}/scripts/gen_ibex_core.sh ${PROJECT_SOURCE_DIR}/sub/ibex ${CMAKE_BINARY_DIR}/codegen/ibex
)

#Add a rule so the core can be regenerated using make gen_ibex_core
add_custom_target(gen_ibex_core
      COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/gen_ibex_core.sh ${PROJECT_SOURCE_DIR}/sub/ibex ${CMAKE_BINARY_DIR}/codegen/ibex
      COMMENT
        "Code generate the ibex core."
      VERBATIM
  )

#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE ibex_top
    COMPONENT_NAME ibex
)

add_test(NAME ibex_no_prefetch_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/ibex_single_prefetch_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
