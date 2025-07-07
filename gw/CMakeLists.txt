#Only relevant for CMAKE_BUILD_TYPE=fpga: -DBL_TARGET_FPGA="arty-a7-100".
set(BL_TARGET_FPGA "arty-a7-100" CACHE STRING "FPGA Target: arty-a7-100")
set_property(CACHE BL_TARGET_FPGA PROPERTY STRINGS "arty-a7-100")

#Map to full part number as expected by Vivado.
if(BL_TARGET_FPGA STREQUAL "arty-a7-100")
  set(PART xc7a100ticsg324-1L)
else()
  message(FATAL_ERROR "Invalid part number: ${BL_TARGET_FPGA}.")
endif()

#A top-level target aggregating all lint targets.
add_custom_target(lint_all ALL)

#This function builds a Vivado command string.
#cmd: the output string variable.
#tcl_script: the tcl script Vivado should execute
#ARGN: Variables argument list to be passed to the tcl script.
function(vivado_cmd cmd tcl_script)
  list(JOIN ARGN " " _tcl_args)
  set(${cmd} "${PROJECT_SOURCE_DIR}/scripts/vivado_wrapper.sh -source ${tcl_script} -tclargs ${_tcl_args}" PARENT_SCOPE)
endfunction()

#Function grouping common vivado gateware build rules.
#TOP_MODULE: name of top module.
#PRJ_COMP_NAME: project or component name, e.g. wbuart32
#PRJ_CONSTRAINTS_FILE_LIST: Project constraints file list, if any.
#MEM_FILE_LIST: Mem file list, if any.
function (gw_common_rules_vivado)
  set(options)
  set(oneValueArgs TOP_MODULE PRJ_COMP_NAME OOC PRJ_CONSTRAINTS_FILE_LIST MEM_FILE_LIST)
  set(multiValueArgs)

  cmake_parse_arguments(gw_common_rules_vivado
      "${options}"
      "${oneValueArgs}"
      "${multiValueArgs}"
      ${ARGN}
  )

  set(_top_module ${gw_common_rules_vivado_TOP_MODULE})
  set(_prj_comp_name ${gw_common_rules_vivado_PRJ_COMP_NAME})
  set(_prj_constraints_file_list ${gw_common_rules_vivado_PRJ_CONSTRAINTS_FILE_LIST})
  set(_ooc ${gw_common_rules_vivado_OOC})
  set(_mem_file_list ${gw_common_rules_vivado_MEM_FILE_LIST})

  # Dummy output which is never actually produced. Anything that depends on
  # this will always be rebuilt.
  add_custom_command(
      OUTPUT always_rebuild
      COMMAND cmake -E echo
      COMMENT
          "always_rebuild dummy command."
  )

  add_custom_command(
          OUTPUT
              ${_prj_comp_name}.vivado_sources
          COMMAND
              ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.vivado_sources ${BL_TARGET_FPGA} ${_prj_comp_name}
          WORKING_DIRECTORY
              ${CMAKE_CURRENT_BINARY_DIR}
          DEPENDS
              always_rebuild
          COMMENT
              "Generating vivado source list."
          VERBATIM
      )

  #Extract any Vivado IPs (.xci files) referenced in the bender manifest
  add_custom_command(
        OUTPUT
            ${_prj_comp_name}.vivado_ip_file_list
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_ip_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.vivado_ip_file_list ${BL_TARGET_FPGA}
        DEPENDS
            always_rebuild
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT
            "Generating Vivado IP file list."
        VERBATIM
    )

  #Build Vivado command string for the vivado_create_project tcl script.
  vivado_cmd(create_project_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_create_project.tcl
                -ooc ${_ooc}
                -part ${PART} -sources ${_prj_comp_name}.vivado_sources
                -vivado_ips ${_prj_comp_name}.vivado_ip_file_list
                -top ${_top_module}
  )

  #If a constraints file list is given, append it to the Vivado command string.
  if (NOT ${_prj_constraints_file_list} STREQUAL "")
    string(APPEND create_project_cmd " -prj_constraints " ${_prj_constraints_file_list})
  endif()
  #If a mem file list is given, append it to the Vivado command string.
  if (NOT ${_mem_file_list} STREQUAL "")
    string(APPEND create_project_cmd " -mem_files " ${_mem_file_list})
  endif()

  #Custom command executing the create_project command string.
  add_custom_command(
      OUTPUT
          ${CMAKE_CURRENT_BINARY_DIR}/project.xpr
      COMMAND
          bash -c ${create_project_cmd}
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS
          ${_prj_comp_name}.vivado_ip_file_list
          ${_prj_comp_name}.vivado_sources
          ${_prj_constraints_file_list}
          ${_mem_file_list}
      COMMENT
          "Creating Vivado project."
      VERBATIM
  )

  #Build Vivado command string for the vivado_synth tcl script.
  vivado_cmd(synth_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_synth.tcl
          -project project.xpr -outputDir ${CMAKE_CURRENT_BINARY_DIR})

  #Custom command executing the vivado synthesis command string.
  add_custom_command(
      OUTPUT
          ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
      COMMAND
          bash -c ${synth_cmd}
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS
          ${CMAKE_CURRENT_BINARY_DIR}/project.xpr
      COMMENT
          "Project or component synthesis."
      VERBATIM
  )

  add_custom_target(${_prj_comp_name}_synth
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS
          ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
      COMMENT
          "Project or component synthesis target."
  )
endfunction()

#Function grouping common verilator gateware build rules.
#TOP_MODULE: name of top module.
#PRJ_COMP_NAME: project or component name, e.g. wbuart32
#MEM_FILE_LIST: Mem file list, if any.
#VERILATOR_FLAGS: CFLAGS and LDFLAGS to use when verilating. Prepend each CFLAG with "-CFLAG" and each LDFLAG with "-LDFLAG".
function (gw_common_rules_verilator)
  set(options)
  set(oneValueArgs TOP_MODULE PRJ_COMP_NAME MEM_FILE_LIST)
  set(multiValueArgs VERILATOR_FLAGS)

  cmake_parse_arguments(gw_common_rules_verilator
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

  set(_top_module ${gw_common_rules_verilator_TOP_MODULE})
  set(_prj_comp_name ${gw_common_rules_verilator_PRJ_COMP_NAME})
  set(_mem_file_list ${gw_common_rules_verilator_MEM_FILE_LIST})
  set(_verilator_flags ${gw_common_rules_verilator_VERILATOR_FLAGS})

  # Dummy output which is never actually produced. Anything that depends on
  # this will always be rebuilt.
  add_custom_command(
      OUTPUT always_rebuild
      COMMAND cmake -E echo
      COMMENT
          "always_rebuild dummy command."
  )

  add_custom_command(
      OUTPUT
          ${_prj_comp_name}.vlts
      COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/bender_get_vlts.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.vlts ${BL_TARGET_FPGA}
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS
          always_rebuild
      COMMENT
          "Generating list of .vlt files."
      VERBATIM
  )

  add_custom_command(
      OUTPUT
          ${_prj_comp_name}.verilator_sources
      COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/bender_gen_verilator_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.verilator_sources ${BL_TARGET_FPGA} ${_prj_comp_name}
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS
          always_rebuild
      COMMENT
          "Generating verilator source list."
      VERBATIM
  )

  add_custom_target(${_prj_comp_name}_lint
      ALL
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/verilator_lint_check.sh ${_top_module} ${_prj_comp_name}.vlts ${_prj_comp_name}.waiver ${_prj_comp_name}.verilator_sources ${_verilator_flags}
      DEPENDS
          ${_prj_comp_name}.vlts
          ${_prj_comp_name}.verilator_sources
      BYPRODUCTS
          ${_prj_comp_name}.waiver
      COMMENT
          "Lint check."
      VERBATIM
  )

  #Include lint target in the lint_all dependency list.
  add_dependencies(lint_all ${_prj_comp_name}_lint)
endfunction()

#Projects before components because reconfigurable modules
#Depend on their reference projects.
add_subdirectory(projects)
add_subdirectory(components)
