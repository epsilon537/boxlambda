# This is a test build demonstrating the Dynamic Function Exchange (DFX) feature.
# DFX is also known as partial FPGA reconfiguration.
# This test build implements BoxLambda SoC where one of the subcomponents (virtual 
# socket 0, or vs0) is a black box whose contents (a partial bitstream) can be 
# live-loaded by sofware running on the system.
#
# The default vs0 component, i.e. the component used in the default project bitstream,
# is a do-nothing stub (vs0_stub). The alternative vs0 component that can be live-loaded
# into the vs0 socket is called vs0_j1b and can be found in the gw/components
# directory.
#
# Please refer to the BoxLambda documentation to see how to live-load a VS0 component.
#
# Calling the gw_project_rules_dfx_vivado() function below with given parameters
# results in the creation of a dfx_test_bit, a dfx_test_load, and a dfx_test_flash_gw 
# target. Building the dfx_test_bit target results in a bitstream file that can be 
# _loaded or _gw_flashed onto the target. This gateware image expects to find a 
# software image on flash, to boot from. The software image that does with this 
# hardware image is sw/projects/dfx_test.

#gw_project_rules_dfx_vivado() function is defined in the parent CMakeLists.txt.
gw_project_rules_dfx_vivado(
    TOP_MODULE boxlambda_top
    PROJECT_NAME dfx_test
    #Location of the virtual socket in the design:
    VS_INST boxlambda_soc_inst/vs0_inst
    VS_DEFAULT_COMPONENT vs0_stub
)

