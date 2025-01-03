if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME i2c_test
      MEM_FILE_TARGET i2c_test_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME i2c_test
      MEM_FILE_TARGET i2c_test_ram
      VERILATOR_CPP_FLAGS
          "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang -I${PROJECT_SOURCE_DIR}/sub/wbi2c/bench/cpp"
      VERILATOR_LD_FLAGS
          -lncurses
  )

  #Add testcase.
  add_test(NAME i2c_test_test
      COMMAND ./Vmodel
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()


