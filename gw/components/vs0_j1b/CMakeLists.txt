#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE  vs0
    COMPONENT_NAME vs0_j1b
)

gw_rm_rules_dfx(
  COMPONENT_NAME vs0_j1b
  VS_INST boxlambda_soc_inst/vs0_inst
  REF_DFX_PROJECT dfx_test
)
