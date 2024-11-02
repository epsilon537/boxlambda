#Function grouping gateware build rules for DFX Reconfigurable Module builds.
#COMPONENT_NAME: name of the DFX virtual socket component to implement.
#VS_INST: DFX virtual socket instance name.
#REF_DFX_PROJECT: Reference DFX project to build the Reconfigurable Module against.
function (gw_rm_rules_dfx)
  set(options)
  set(oneValueArgs COMPONENT_NAME VS_INST REF_DFX_PROJECT)
  set(multiValueArgs)

  cmake_parse_arguments(gw_rm_rules_dfx
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

  set(_comp_name ${gw_rm_rules_dfx_COMPONENT_NAME})
  set(_vs_inst ${gw_rm_rules_dfx_VS_INST})
  set(_ref_dfx_prj ${gw_rm_rules_dfx_REF_DFX_PROJECT})

  #Build Vivado command string for the vivado_impl tcl script.
  vivado_cmd(impl_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_impl_dfx_rm.tcl
        -vsCellInst ${_vs_inst}
        -vsDcp $<TARGET_PROPERTY:${_comp_name}_synth,BINARY_DIR>/project.runs/synth_1/*.dcp
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
            ${_comp_name}_synth
            ${_ref_dfx_prj}_bit
            $<TARGET_PROPERTY:${_ref_dfx_prj}_bit,BINARY_DIR>/dfx_project.static_route.dcp
        COMMENT
            "Implementing DFX Reconfigurable Module and generating bitstream(s)."
        VERBATIM
    )

  #Add the bitstream generation target to make all (ALL property).
  add_custom_target(${_comp_name}_bit
        ALL
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
add_subdirectory(picorv_dma)
add_subdirectory(boxlambda_clk_gen)
add_subdirectory(wbxbar)
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

