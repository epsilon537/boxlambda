#Only relevant for CMAKE_BUILD_TYPE=fpga: -DBL_TARGET_FPGA="arty-a7-35" or -DBL_TARGET_FPGA="arty-a7-100".
set(BL_TARGET_FPGA "arty-a7-35" CACHE STRING "FPGA Target: arty-a7-35 or arty-a7-100")
set_property(CACHE BL_TARGET_FPGA PROPERTY STRINGS "arty-a7-35" "arty-a7-100")

#Map to full part number as expected by Vivado.
if(BL_TARGET_FPGA STREQUAL "arty-a7-35")
    set(PART xc7a35ticsg324-1L)
elseif(BL_TARGET_FPGA STREQUAL "arty-a7-100")
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
    set(${cmd} "vivado -nolog -nojournal -mode batch -notrace -source ${tcl_script} -tclargs ${_tcl_args}" PARENT_SCOPE)
endfunction()

#Function grouping common gateware build rules.
#TOP_MODULE: name of top module.
#PRJ_COMP_NAME: project or component name, e.g. wbuart32
#CONSTRAINTS_FILE_LIST: Constraints file list, if any. 
#MEM_FILE_LIST: Mem file list, if any.
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty. 
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty. 
function (gw_common_rules)
    set(options)
    set(oneValueArgs TOP_MODULE PRJ_COMP_NAME CONSTRAINTS_FILE_LIST MEM_FILE_LIST VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
    set(multiValueArgs)
    
    cmake_parse_arguments(gw_common_rules
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    set(_top_module ${gw_common_rules_TOP_MODULE}) 
    set(_prj_comp_name ${gw_common_rules_PRJ_COMP_NAME})
    set(_constraints_file_list ${gw_common_rules_CONSTRAINTS_FILE_LIST})
    set(_mem_file_list ${gw_common_rules_MEM_FILE_LIST})
    set(_verilator_cpp_flags ${gw_common_rules_VERILATOR_CPP_FLAGS})
    set(_verilator_ld_flags ${gw_common_rules_VERILATOR_LD_FLAGS})

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
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_verilator_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.verilator_sources ${CMAKE_CURRENT_BINARY_DIR}/Vmodel ${BL_TARGET_FPGA} ${_prj_comp_name}
        BYPRODUCTS
            ${_prj_comp_name}.verilator_sources.dep
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS
            always_rebuild
        COMMENT
            "Generating verilator source list."
        VERBATIM
    )

    add_custom_target(${_prj_comp_name}_lint
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/verilator_lint_check.sh ${_top_module} ${_prj_comp_name}.vlts ${_prj_comp_name}.waiver ${_prj_comp_name}.verilator_sources ${_verilator_cpp_flags} ${_verilator_ld_flags}
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

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        add_custom_command(
            OUTPUT
                ${_prj_comp_name}.vivado_sources
            BYPRODUCTS
                ${CMAKE_CURRENT_BINARY_DIR}/${_prj_comp_name}.vivado_sources.dep
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.vivado_sources ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp ${BL_TARGET_FPGA} ${_prj_comp_name}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                always_rebuild
            COMMENT
                "Generating vivado source list."
            VERBATIM
        )

        #Build Vivado command string for the vivado_create_project tcl script.
        vivado_cmd(create_project_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_create_project.tcl
                    -project project.xpr 
                    -part ${PART} -sources ${_prj_comp_name}.vivado_sources
                    -outputDir ${CMAKE_CURRENT_BINARY_DIR} -top ${_top_module})

        #If a constraints file list is given, append it to the Vivado command string.
        if (NOT ${_constraints_file_list} STREQUAL "")
            string(APPEND create_project_cmd " -constraints " ${_constraints_file_list})
        endif()
        #If a mem file list is given, append it to the Vivado command string.
        if (NOT ${_mem_file_list} STREQUAL "")
            string(APPEND create_project_cmd " -mem_files " ${_mem_file_list})
        endif()

        #Custom command executing the create_project command string.
        #Notice that a dummy project.dep output is generated along with the actual project.xpr output.
        #This is because project.xpr modification time updates every time we open the project file for synthesis or
        #implementation. We can't use project.xpr as a dependency to detect if a re-synthesis is needed.
        #project.dep on the other hand is only touched when the Vivado project file is actually created, so that one
        #can be used as a dependency by the synthesis target.
        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.dep
            COMMAND
                bash -c ${create_project_cmd}
            COMMAND
                touch ${CMAKE_CURRENT_BINARY_DIR}/project.dep
            BYPRODUCTS
                ${CMAKE_CURRENT_BINARY_DIR}/project.xpr
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${_prj_comp_name}.vivado_sources
                ${_constraints_file_list}
                ${_mem_file_list}
            COMMENT
                "Creating Vivado project."
            VERBATIM
        )

        #Build Vivado command string for the vivado_synth tcl script.
        vivado_cmd(synth_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_synth.tcl
            -project project.xpr -outputDir $OUTDIR ${CMAKE_CURRENT_BINARY_DIR})

        #Custom command executing the vivado synthesis command string.
        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
            COMMAND
                bash -c ${synth_cmd}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.dep
            DEPFILE
                ${CMAKE_CURRENT_BINARY_DIR}/${_prj_comp_name}.vivado_sources.dep
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
    endif()   
endfunction()

add_subdirectory(components)
add_subdirectory(projects)
