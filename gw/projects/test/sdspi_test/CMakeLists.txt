if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME sdspi_test
      MEM_FILE_TARGET sdspi_test
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME sdspi_test
      MEM_FILE_TARGET sdspi_test
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/sdspi/bench/cpp"
  )

  #Add testcase.
  add_test(NAME sdspi_test_test
      COMMAND ./Vmodel -s ${CMAKE_CURRENT_LIST_DIR}/test/sdcard.img
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

  #This test requires a longer timeout.
  set_tests_properties(sdspi_test_test PROPERTIES TIMEOUT 400)
endif()
