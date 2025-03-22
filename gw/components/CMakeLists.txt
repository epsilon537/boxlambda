#Function grouping gateware build rules for DFX Reconfigurable Module builds.
#COMPONENT_NAMES: names of all the DFX virtual socket components that are part of the design.
#List the 'current' component first.
#VS_INSTS: DFX virtual socket instance names, one for each COMPONENT_NAME specified.
#REF_DFX_PROJECT: Reference DFX project to build the Reconfigurable Module against.
function (gw_rm_rules_dfx)
  set(options)
  set(oneValueArgs REF_DFX_PROJECT)
  set(multiValueArgs COMPONENT_NAMES VS_INSTS)

  cmake_parse_arguments(gw_rm_rules_dfx
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

  set(_comp_names ${gw_rm_rules_dfx_COMPONENT_NAMES})
  list(GET _comp_names 0 _comp_name)
  set(_vs_insts ${gw_rm_rules_dfx_VS_INSTS})
  set(_ref_dfx_prj ${gw_rm_rules_dfx_REF_DFX_PROJECT})

  set(dcps "")
  set(comps_synth "")
  foreach(comp ${_comp_names})
    list(APPEND dcps $<TARGET_PROPERTY:${comp}_synth,BINARY_DIR>/project.runs/synth_1/*.dcp)
    list(APPEND comps_synth ${comp}_synth)
  endforeach()

  #Build Vivado command string for the vivado_impl tcl script.
  vivado_cmd(impl_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_impl_dfx_rm.tcl
        -vsCellInsts \"${_vs_insts}\"
        -vsDcps \"${dcps}\"
        -staticDcp $<TARGET_PROPERTY:${_ref_dfx_prj}_bit,BINARY_DIR>/dfx_project.static_route.dcp
        -outputBaseName ${_comp_name}
        -part ${PART})

  #Custom command executing the vivado implementation command string.
  add_custom_command(
        OUTPUT
            ${_comp_name}.bin
        COMMAND
            bash -c ${impl_cmd}
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS
            ${comps_synth}
            ${_ref_dfx_prj}_bit
            $<TARGET_PROPERTY:${_ref_dfx_prj}_bit,BINARY_DIR>/dfx_project.static_route.dcp
        COMMENT
            "Implementing DFX Reconfigurable Module and generating bitstream(s)."
        VERBATIM
    )

  add_custom_target(${_comp_name}_bit
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS
            ${_comp_name}.bin
        COMMENT
            "DFX Reconfigurable Module implementation and bitstream generation target."
    )
endfunction()

#Function grouping gateware component build rules.
#TOP_MODULE: name of top module.
#COMPONENT_NAME: component name, e.g. wbuart32
function (gw_component_rules)
  set(options)
  set(oneValueArgs TOP_MODULE COMPONENT_NAME)
  set(multiValueArgs)

  cmake_parse_arguments(gw_component_rules
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

  set(_top_module ${gw_component_rules_TOP_MODULE})
  set(_comp_name ${gw_component_rules_COMPONENT_NAME})

  if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    #Forward to gw_common_rules() function. gw_common_rules() build logic is shared between
    #gw components and projects.
    gw_common_rules_vivado(
          TOP_MODULE
              ${_top_module}
          PRJ_COMP_NAME
              ${_comp_name}
          OOC
              "1"
      )
  else()
    #Forward to gw_common_rules() function. gw_common_rules() build logic is shared between
    #gw components and projects.
    gw_common_rules_verilator(
          TOP_MODULE
              ${_top_module}
          PRJ_COMP_NAME
              ${_comp_name}
      )
  endif()
endfunction()

#Gateware components:
add_subdirectory(audio_dac)
add_subdirectory(ym2149)
add_subdirectory(sdspi)
add_subdirectory(vera)
add_subdirectory(wbuart)
add_subdirectory(riscv-dbg)
add_subdirectory(gpio)
add_subdirectory(litedram)
add_subdirectory(ibex_wb_core)
add_subdirectory(ibex)
add_subdirectory(reset_ctrl)
add_subdirectory(boxlambda_clk_gen)
add_subdirectory(interconnect)
add_subdirectory(wb_dp_ram)
add_subdirectory(boxlambda_soc)
add_subdirectory(vs0_stub)
add_subdirectory(vs0_j1b)
add_subdirectory(usb_hid_host)
add_subdirectory(usb_hid_device)
add_subdirectory(cdc_cells)
add_subdirectory(spiflash)
add_subdirectory(wb_timer)
add_subdirectory(i2c_master)
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  add_subdirectory(wb_dfx_controller)
endif()

