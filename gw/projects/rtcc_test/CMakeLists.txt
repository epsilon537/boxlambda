if(CMAKE_BUILD_TYPE STREQUAL "vivado")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME rtcc_test
      MEM_FILE_TARGET cmem_to_flash_vector #rtcc_test_ram
  )
endif()
