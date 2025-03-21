#code generate the litedram core.
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  execute_process(
      COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/gen_litedram_core.sh ${PROJECT_SOURCE_DIR}/scripts/artya7dram.yml ${CMAKE_BINARY_DIR}/codegen/litedram
  )

  #Add a rule so the core can be regenerated using make gen_litedram_core
  add_custom_target(gen_litedram_core
        COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/gen_litedram_core.sh ${PROJECT_SOURCE_DIR}/scripts/artya7dram.yml ${CMAKE_BINARY_DIR}/codegen/litedram
        COMMENT
          "Code generate the litedram core."
        VERBATIM
    )
else()
  execute_process(
      COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/gen_litedram_core.sh ${PROJECT_SOURCE_DIR}/scripts/artya7dram.yml ${CMAKE_BINARY_DIR}/codegen/litedram --sim
  )

  #Add a rule so the core can be regenerated using make gen_litedram_core
  add_custom_target(gen_litedram_core
        COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/gen_litedram_core.sh ${PROJECT_SOURCE_DIR}/scripts/artya7dram.yml ${CMAKE_BINARY_DIR}/codegen/litedram --sim
        COMMENT
          "Code generate the litedram core."
        VERBATIM
    )
endif()

#gw_component_rules function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE litedram_wrapper
    COMPONENT_NAME litedram
)
