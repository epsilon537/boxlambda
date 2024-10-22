# The Dynamic Function eXchange (DFX) Controller Xilinx IP provides management functions for DFX designs (designs
# that support partial FPGA reconfiguration).
# When software trigger events occur, the DFX Controller pulls partial bitstreams from memory and delivers them to an
# internal configuration access port (ICAP).  The IP also assists with logical decoupling and startup events,
# customizable per Reconfigurable Partition.
#
# wb_dfx_controller is a DFX controller with a Wishbone frontend.

#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE wb_dfx_controller
    COMPONENT_NAME wb_dfx_controller
)
