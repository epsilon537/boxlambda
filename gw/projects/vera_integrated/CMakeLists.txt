if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME vera_integrated
      MEM_FILE_TARGET vera_test
  )
else()
  #Get the SDL2 compile flags
  execute_process(
    COMMAND
      bash -c "sdl2-config --cflags"
    OUTPUT_VARIABLE
      SDL2_CFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  #Get the SDL2 link flags
  execute_process(
    COMMAND
      bash -c "sdl2-config --libs"
    OUTPUT_VARIABLE
      SDL2_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME vera_integrated
      MEM_FILE_TARGET vera_test
      VERILATOR_CPP_FLAGS
          "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/ -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang ${SDL2_CFLAGS}"
      VERILATOR_LD_FLAGS
          "${SDL2_LIBS}"
  )

  #Add testcase. This one's a script, not just the Verilator Vmodel executable.
  add_test(NAME vera_integrated_test
      COMMAND ${CMAKE_CURRENT_LIST_DIR}/test/test.sh ${PROJECT_SOURCE_DIR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

  #This test requires a longer timeout.
  set_tests_properties(vera_integrated_test PROPERTIES TIMEOUT 200)
endif()
