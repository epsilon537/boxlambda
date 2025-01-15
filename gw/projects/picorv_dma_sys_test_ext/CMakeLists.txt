if(CMAKE_BUILD_TYPE STREQUAL "vivado")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME picorv_dma_sys_test_ext
      MEM_FILE_TARGET picorv_dma_sys_test_ext_ram
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME picorv_dma_sys_test_ext
      MEM_FILE_TARGET picorv_dma_sys_test_ext_ram
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/" 
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang" 
  )

  #Add testcase.
  add_test(NAME picorv_dma_sys_test_ext
      COMMAND ./Vmodel
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()
