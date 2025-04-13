if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME vera_integrated
      MEM_FILE_TARGET vera_test_ram
  )
else()
  find_program(SDL2_CONFIG_EXECUTABLE NAMES sdl2-config)
  if(SDL2_CONFIG_EXECUTABLE)
    message(STATUS "sdl2-config found: ${SDL2_CONFIG_EXECUTABLE}")
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

    #A clumsy way of prepending each SDL2 CFLAG with '-CFLAGS' for verilator:

    #Convert the string to a list by replacing space with ;
    string(REPLACE " " ";" SDL2_CFLAGS_LIST ${SDL2_CFLAGS})

    #Iterate over the list, prepend each item, and accumulate into  a new list.
    set(SDL2_CFLAGS_PREPENDED "")
    foreach(cflag ${SDL2_CFLAGS_LIST})
      string(PREPEND cflag "-CFLAGS ")
      list(APPEND SDL2_CFLAGS_PREPENDED ${cflag})
    endforeach()

    #Repeat for SDL2 libs:

    string(REPLACE " " ";" SDL2_LIBS_LIST ${SDL2_LIBS})

    set(SDL2_LIBS_PREPENDED "")
    foreach(lib ${SDL2_LIBS_LIST})
      string(PREPEND lib "-LDFLAGS ")
      list(APPEND SDL2_LIBS_PREPENDED ${lib})
    endforeach()

    gw_project_rules_verilator(
        TOP_MODULE sim_main
        PROJECT_NAME vera_integrated
        MEM_FILE_TARGET vera_test_ram
        VERILATOR_FLAGS
            "-CFLAGS -DSDL2_SUPPORT"
            "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
            "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
            ${SDL2_CFLAGS_PREPENDED}
            ${SDL2_LIBS_PREPENDED}
    )
  else()
    message(STATUS "sdl2-config was not found. Proceeding without SDLsupport.")

    gw_project_rules_verilator(
        TOP_MODULE sim_main
        PROJECT_NAME vera_integrated
        MEM_FILE_TARGET vera_test_ram
        VERILATOR_FLAGS
            "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
            "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
    )
  endif()

  #Add testcase. This one's a script, not just the Verilator Vmodel executable.
  add_test(NAME vera_integrated_test
      COMMAND ${CMAKE_CURRENT_LIST_DIR}/test/test.sh ${PROJECT_SOURCE_DIR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

  #This test requires a longer timeout.
  set_tests_properties(vera_integrated_test PROPERTIES TIMEOUT 200)
endif()
