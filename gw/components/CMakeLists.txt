#Function grouping gateware component build rules.
#TOP_MODULE: name of top module when synthesizing or linting component "Out Of Context" (OOC). Standalone in other words.
#COMPONENT_NAME: name of component, e.g. wbuart32
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty. 
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty. 
function (gw_component_rules)
    set(options)
    set(oneValueArgs NAME TOP_MODULE COMPONENT_NAME VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
    set(multiValueArgs)
    cmake_parse_arguments(gw_component_rules
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    set(_top_module ${gw_component_rules_TOP_MODULE}) 
    set(_prj_comp_name ${gw_component_rules_COMPONENT_NAME}) 
    set(_verilator_cpp_flags ${gw_component_rules_VERILATOR_CPP_FLAGS}) 
    set(_verilator_ld_flags ${gw_component_rules_VERILATOR_LD_FLAGS})

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
            ${PROJECT_SOURCE_DIR}/scripts/bender_get_vlts.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.vlts
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
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_verilator_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.verilator_sources "" ${_prj_comp_name}
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
                ${CMAKE_CURRENT_BINARY_DIR}/${_prj_comp_name}.vivado_sources
            BYPRODUCTS
                ${CMAKE_CURRENT_BINARY_DIR}/${_prj_comp_name}.vivado_sources.dep
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.vivado_sources ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp ${_prj_comp_name}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                always_rebuild
            COMMENT
                "Generating vivado source list."
            VERBATIM
        )

        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.dep
            BYPRODUCTS
                ${CMAKE_CURRENT_BINARY_DIR}/project.xpr
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_create_project.sh ${PART} ${_prj_comp_name}.vivado_sources "" "" ${CMAKE_CURRENT_BINARY_DIR} ${_top_module}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/${_prj_comp_name}.vivado_sources
            COMMENT
                "Creating Vivado project."
            VERBATIM
        )

        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_synth.sh ${CMAKE_CURRENT_BINARY_DIR}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.dep
            DEPFILE
                ${CMAKE_CURRENT_BINARY_DIR}/${_prj_comp_name}.vivado_sources.dep
            COMMENT
                "Component OOC synthesis."
            VERBATIM
        )

        add_custom_target(${_prj_comp_name}_synth
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
            COMMENT
                "Component OOC synthesis target."
        )

        #A bunch of generated files to clean up when running make clean.
        set_target_properties(${_prj_comp_name}_synth PROPERTIES ADDITIONAL_CLEAN_FILES 
            "project.cache;project.runs;project.xpr;project.ip_user_files;project.hw;syn_timing.rpt;syn_util.rpt")
    endif()
endfunction()

#Gateware components:
add_subdirectory(wbuart32)
add_subdirectory(riscv-dbg)
add_subdirectory(wb_gpio)
add_subdirectory(litedram)
add_subdirectory(ibex_wb_core)
add_subdirectory(ibex)


