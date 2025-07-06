# This is the DFX enabled configuration of BoxLambda.

# DFX is also known as partial FPGA reconfiguration.
# This build implements BoxLambda SoC where one of the subcomponents (virtual
# socket 0, or vs0) is a black box whose contents (a partial bitstream) can be
# dynamically loaded by sofware running on the system.
#
# The default vs0 component, i.e. the component used in the default project bitstream,
# is a do-nothing stub (vs0_stub). An alternative vs0 component that can be dynamically loaded
# into the vs0 socket is called vs0_j1b and can be found in the gw/components
# directory.
#
# Please refer to the BoxLambda documentation to see how to dynamically load a VS0 component.
#
# Calling the gw_project_rules_dfx_vivado() function below with given parameters
# results in the creation of a boxlambda_dfx_bit, a boxlambda_dfx_load, and a boxlambda_flash_gw
# build target. Building the boxlambda_dfx_bit target results in a bitstream file that can be
# _loaded or _gw_flashed onto the target device. This gateware image expects to find a
# software image on flash to boot from.

if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  #gw_project_rules_dfx_vivado() function is defined in the parent CMakeLists.txt.
  gw_project_rules_dfx_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME boxlambda_dfx
      #Location of the virtual socket in the design:
      VS_INSTS
        boxlambda_soc_inst/GENERATE_VS0_MODULE.vs0_inst
      VS_DEFAULT_COMPONENTS
        vs0_stub
  )

  #Add to ALL list.
  add_custom_target(boxlambda_dfx_all_wrapper ALL)
  add_dependencies(boxlambda_dfx_all_wrapper boxlambda_dfx_bit)
else()
  #gw_project_rules_verilator() function is defined in the parent CMakeLists.txt.
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME boxlambda_dfx
      MEM_FILE_TARGET dfx_test
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
  )

  #Add testcase.
  add_test(NAME boxlambda_dfx_test
      COMMAND ./Vmodel
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()

