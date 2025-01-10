if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME ym2149_dac_test
      MEM_FILE_TARGET ym2149_test_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME ym2149_dac_test
      MEM_FILE_TARGET ym2149_test_ram
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/" 
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
  )

  #Add testcase. This one's a script, not just the Verilator Vmodel executable.
  add_test(NAME ym2149_dac_test
      COMMAND ${CMAKE_CURRENT_LIST_DIR}/test/test.sh ${PROJECT_SOURCE_DIR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()
