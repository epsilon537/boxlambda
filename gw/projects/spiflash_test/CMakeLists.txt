if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME spiflash_test
      MEM_FILE_TARGET cmem_to_flash_vector
  )
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME spiflash_test
      MEM_FILE_TARGET cmem_to_flash_vector
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/gw/components/spiflash/sim/"
  )

  add_dependencies(spiflash_test_sim_sw spiflash_test_flsh)

  #Add testcase.
  add_test(NAME spiflash_test_test
      COMMAND ./Vmodel -f ../../../sw/projects/spiflash_test/spiflash_test_flsh.bin
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()
