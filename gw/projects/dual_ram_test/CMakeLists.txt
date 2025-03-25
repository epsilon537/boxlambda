if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME dual_ram_test
      MEM_FILE_TARGET dual_ram_test_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME dual_ram_test
      MEM_FILE_TARGET dual_ram_test_ram
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
  )

  #Add testcase.
  add_test(NAME dual_ram_test_test
      COMMAND ./Vmodel
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()

