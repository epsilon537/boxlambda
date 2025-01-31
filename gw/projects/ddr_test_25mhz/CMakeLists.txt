if(CMAKE_BUILD_TYPE STREQUAL "vivado")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME ddr_test_25mhz
      MEM_FILE_TARGET ddr_test_25mhz_ram
  )
elseif(CMAKE_BUILD_TYPE STREQUAL "openxc7")
  #gw_project_rules_openxc7() function is defined in the parent CMakeLists.txt.
  gw_project_rules_openxc7(
      TOP_MODULE boxlambda_top
      PROJECT_NAME ddr_test_25mhz
      FREQ 25
      MEM_FILE_TARGET ddr_test_25mhz_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME ddr_test_25mhz
      MEM_FILE_TARGET ddr_test_25mhz_ram
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
  )

  #Add testcase
  add_test(NAME ddr_test_25mhz_test
      COMMAND ./Vmodel
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

  #This test requires a longer timeout.
  set_tests_properties(ddr_test_25mhz_test PROPERTIES TIMEOUT 200)
endif()
