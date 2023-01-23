#Function grouping gateware component build rules.
#TOP_MODULE: name of top module when synthesizing or linting component "Out Of Context" (OOC). Standalone in other words.
#OOC_TARGET: name of Bender target set when synthesizing or linting component "Out Of Context" (OOC). Standalone in other words.
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty. 
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty. 
function (gw_component_rules)
    set(options)
    set(oneValueArgs NAME TOP_MODULE OOC_TARGET VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
    set(multiValueArgs)
    cmake_parse_arguments(gw_component_rules
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    set(_top_module ${gw_component_rules_TOP_MODULE}) 
    set(_ooc_target ${gw_component_rules_OOC_TARGET}) 
    set(_verilator_cpp_flags ${gw_component_rules_VERILATOR_CPP_FLAGS}) 
    set(_verilator_ld_flags ${gw_component_rules_VERILATOR_LD_FLAGS})

    # Dummy output which is never actually produced. Anything that depends on
    # this will always be rebuilt.
    add_custom_command(
        OUTPUT always_rebuild
        COMMAND cmake -E echo
    )

    add_custom_command(
        OUTPUT
            ${_ooc_target}.vlts
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_get_vlts.sh ${CMAKE_CURRENT_LIST_DIR} ${_ooc_target}.vlts
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
            ${_ooc_target}.verilator_script
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_verilator_script.sh ${CMAKE_CURRENT_LIST_DIR} ${_ooc_target}.verilator_script ${_ooc_target}
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS
            always_rebuild
        COMMENT
            "Generating verilator script."
        VERBATIM
    )

    add_custom_target(${_ooc_target}_lint
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/verilator_lint_check.sh ${_top_module} ${_ooc_target}.vlts ${_ooc_target}.waiver ${_ooc_target}.verilator_script ${_verilator_cpp_flags} ${_verilator_ld_flags}
        DEPENDS
            ${_ooc_target}.vlts
            ${_ooc_target}.verilator_script
        BYPRODUCTS 
            ${_ooc_target}.waiver
        COMMENT
            "Lint check."
        VERBATIM
    )

    #Include lint target in the lint_all dependency list.
    add_dependencies(lint_all ${_ooc_target}_lint)

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        add_custom_command(
            OUTPUT
                ${_ooc_target}.vivado_script
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_script.sh ${CMAKE_CURRENT_LIST_DIR} ${_ooc_target}.vivado_script ${_ooc_target}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                always_rebuild
            COMMENT
                "Generating vivado script."
            VERBATIM
        )

        add_custom_target(${_ooc_target}_synth
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_cmd.sh synth ${PART} ${_ooc_target}.vivado_script "" "" ${CMAKE_CURRENT_BINARY_DIR} ${_top_module}
            DEPENDS
                ${_ooc_target}.vivado_script
            COMMENT
                "Component OOC synthesis."
            VERBATIM
        )

        #A bunch of generated files to clean up when running make clean.
        set_target_properties(${_ooc_target}_synth PROPERTIES ADDITIONAL_CLEAN_FILES 
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


