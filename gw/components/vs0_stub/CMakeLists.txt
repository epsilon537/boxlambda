# This is a stub Reconfigurable Module fitting in Virtual Socket 0 (VS0).
# VS0 resides on the crossbar with a Wishbone master and slave port, and irq inputs and output.
# Reconfigurable Modules and Virtual sockets are part of the Dynamic Function
# Exchange (DFX) a.k.a. Partial FPGA Reconfiguration framework. Refer to
# BoxLambda's documentation for more info.
# A stub is an almost-but-not entirely empty module. All it does is properly terminate
# the VS0 interface signals, so they aren't left dangling. Wishbone reads and writes to
# the module are acknowledged but don't trigger any functionality. All wishbone read
# operations from the VS0 address range return the stub module's signature value.

#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE  vs0
    COMPONENT_NAME vs0_stub
)

if(CMAKE_BUILD_TYPE STREQUAL "fpga")
if(BL_TARGET_FPGA STREQUAL "arty-a7-100")
  # vs0_stub is a DFX component. Next to lint check and synthesis, defined in gw_component_rules(), a DFX Reconfigurable
  # Module needs to be implemented and turned into a bitsteam that can be live-loaded onto a DFX-enabled system. The
  # build rules to do that are defined in the gw_rm_rules_dfx() function. Calling this function with the parameters
  # given below results in a vs0_stub_bit target being defined. Building that target results in a DFX partial bitstream
  # of the vs0_stub component.
  gw_rm_rules_dfx(
    COMPONENT_NAME vs0_stub
    VS_INST boxlambda_soc_inst/vs0_inst
    REF_DFX_PROJECT dfx_test
  )
endif()
endif()
