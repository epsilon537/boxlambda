set(BL_TARGET_FPGA "arty-a7-35" CACHE STRING "FPGA Target: arty-a7-35 or arty-a7-100")
set_property(CACHE BL_TARGET_FPGA PROPERTY STRINGS "arty-a7-35" "arty-a7-100")

if(BL_TARGET_FPGA STREQUAL "arty-a7-35")
    set(PART xc7a35ticsg324-1L)
elseif(BL_TARGET_FPGA STREQUAL "arty-a7-100")
    set(PART xc7a100ticsg324-1L)
else()
    message(FATAL_ERROR "Invalid part number: ${BL_TARGET_FPGA}.")
endif()

function (gw_component_rules _top_module _ooc_target _verilator_cpp_flags _verilator_ld_flags)
    add_custom_command(
        OUTPUT
            ${_ooc_target}.vlts
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_get_vlts.sh ${CMAKE_CURRENT_LIST_DIR} ${_ooc_target}.vlts
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
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
            "Lint check target."
        VERBATIM
    )

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        add_custom_command(
            OUTPUT
                ${_ooc_target}.vivado_script
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_script.sh ${CMAKE_CURRENT_LIST_DIR} ${_ooc_target}.vivado_script ${_ooc_target}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
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
                "Component OOC synthesis target."
            VERBATIM
        )

        set_target_properties(${_ooc_target}_synth PROPERTIES ADDITIONAL_CLEAN_FILES 
            "project.cache;project.runs;project.xpr;project.ip_user_files;project.hw;syn_timing.rpt;syn_util.rpt")
    endif()
endfunction()

add_subdirectory(components/wbuart32)
