if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME hello_world
      MEM_FILE_TARGET hello_world_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME hello_world
      MEM_FILE_TARGET hello_world_ram
      VERILATOR_CPP_FLAGS
          "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
      VERILATOR_LD_FLAGS
          -lncurses
  )

  #Add testcase.
  add_test(NAME hello_world_test
      COMMAND ./Vmodel
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

  #Add testcase. This one's a script, not just the Verilator Vmodel executable.
  add_test(NAME hello_dbg_test
      COMMAND ${CMAKE_CURRENT_LIST_DIR}/test/test.sh ${PROJECT_SOURCE_DIR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()
