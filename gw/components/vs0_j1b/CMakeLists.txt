# This is a J1B realization of a Reconfigurable Module fitting in Virtual Socket 0 VS0). 
# VS0 resides on the crossbar with a Wishbone master and slave port, and irq inputs and output.
# Reconfigurable Modules and Virtual sockets are part of the Dynamic Function
# Exchange (DFX) a.k.a. Partial FPGA Reconfiguration framework. Refer to
# BoxLambda's documentation for more info.
# J1B is is a 32 bit, minimum instruction set stack processor, in other words a Forth CPU.
# J1B is the CPU component of the SwapForth environment.

#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE  vs0
    COMPONENT_NAME vs0_j1b
)

if(CMAKE_BUILD_TYPE STREQUAL "fpga")
if(BL_TARGET_FPGA STREQUAL "arty-a7-100")
  # vs0_J1B is a DFX component. Next to lint check and synthesis, defined in gw_component_rules(), a DFX Reconfigurable
  # Module needs to be implemented and turned into a bitsteam that can be dynamically loaded onto a DFX-enabled system. The
  # build rules to do that are defined in the gw_rm_rules_dfx() function. Calling this function with the parameters
  # given below results in a vs0_j1b_bit target being defined. Building that target results in a DFX partial bitstream
  # of the vs0_j1b component.
  gw_rm_rules_dfx(
    COMPONENT_NAMES
      vs0_j1b
    #Specify where in the SoC this component plugs into.
    VS_INSTS
      boxlambda_soc_inst/GENERATE_VS0_MODULE.vs0_inst
    #Specify the reference project defining the static portion of the DFX build.
    REF_DFX_PROJECT boxlambda_dfx
  )
endif()
endif()
