# This is the 'official' base configuration of BoxLambda (i.e. without the configuration without DFX support).
#
# This is an FPGA-only project build. Most sibling *_test project builds build a variant of this project, geared towards testing a specific feature, and include a Verilator version as well.
# This build expects to find the software image to boot in flash memory.

if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME boxlambda_base
      MEM_FILE_TARGET cmem_to_flash_vector
  )
endif()

