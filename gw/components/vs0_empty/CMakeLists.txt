# This is an empty component placeholder for Virtual Socket 0.
# VS0 resides on the crossbar with a Wishbone master and slave port,
# and irq inputs and output.

# Synthesis of the static portion of a DFX (Dynamic Function Exchange,
# a.k.a. Partial FPGA Reconfiguration) build expects an empty module
# for each virtual socket (reconfigurable partition) in the design, to nail
# down the interface of that virtual socke.
# In the BoxLambda build system, the DFX enabled build project (e.g. dfx_test)
# pulls in the empty module component as a Bender dependency.

# Note that the empty component is only used in the synthesis step of
# a DFX enabled project. All other configurations using the boxlambda_soc
# component (verilator, lint checker, non-DFX builds) reference a non-empty
# virtual socket variant component such as vs0_stub or vs0_j1b.

#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE vs0
    COMPONENT_NAME vs0_empty
)
