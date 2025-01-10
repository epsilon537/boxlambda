if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME fatfs_test
      MEM_FILE_TARGET fatfs_test_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME fatfs_test
      MEM_FILE_TARGET fatfs_test_ram
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/sdspi/bench/cpp"
  )

  #Add testcase.
  add_test(NAME fatfs_test_test
      COMMAND ./Vmodel -s ${CMAKE_CURRENT_LIST_DIR}/test/sdcard.img
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endif()

