#Add a rule to code-generate wb_muxes and arbiters
add_custom_target(gen_wb_mux_arbiters
    COMMAND
      rm -rf ${CMAKE_BINARY_DIR}/codegen/interconnect
    COMMAND
      mkdir -p ${CMAKE_BINARY_DIR}/codegen/interconnect
    COMMAND
      python3 ${PROJECT_SOURCE_DIR}/sub/verilog-wishbone/rtl/wb_mux.py -p 4 -n wb_mux_4 -o ${CMAKE_BINARY_DIR}/codegen/interconnect/wb_mux_4.v
    COMMAND
      python3 ${PROJECT_SOURCE_DIR}/sub/verilog-wishbone/rtl/wb_mux.py -p 18 -n wb_mux_18 -o ${CMAKE_BINARY_DIR}/codegen/interconnect/wb_mux_18.v
    COMMAND
      python3 ${PROJECT_SOURCE_DIR}/sub/verilog-wishbone/rtl/wb_arbiter.py -p 2 -n wb_arbiter_2 -o ${CMAKE_BINARY_DIR}/codegen/interconnect/wb_arbiter_2.v
    COMMAND
      python3 ${PROJECT_SOURCE_DIR}/sub/verilog-wishbone/rtl/wb_arbiter.py -p 3 -n wb_arbiter_3 -o ${CMAKE_BINARY_DIR}/codegen/interconnect/wb_arbiter_3.v
    COMMENT
      "Code generate wb_mux and arbiter variants."
    VERBATIM
)

#Add to the gateware codegeneration dependency list.
add_dependencies(gw_codegen gen_wb_mux_arbiters)

#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE interconnect_ooc
    COMPONENT_NAME interconnect
)

add_test(NAME wb_arbiter_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/wb_arbiter_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)


