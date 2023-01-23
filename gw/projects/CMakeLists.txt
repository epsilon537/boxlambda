#Function grouping gateware project build rules.
#TOP_MODULE: name of top module when synthesizing or linting component "Out Of Context" (OOC). Standalone in other words.
#PROJECT_NAME: project name, e.g. hello_dbg
#MEM_FILE_TARGET: name of the SW target that generates the required .mem file, e.g. picolibc_test
#MEM_FILE_OUT: .mem file created by this function, and expected by the project build, e.g. spram.mem
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty. 
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty. 
function (gw_project_rules)
    set(options)
    set(oneValueArgs NAME TOP_MODULE PROJECT_NAME MEM_FILE_TARGET MEM_FILE_OUT VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
    set(multiValueArgs)
    
    cmake_parse_arguments(gw_project_rules
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    set(_top_module ${gw_project_rules_TOP_MODULE}) 
    set(_prj_name ${gw_project_rules_PROJECT_NAME})
    set(_mem_file_tgt ${gw_project_rules_MEM_FILE_TARGET}) 
    set(_mem_file_out ${gw_project_rules_MEM_FILE_OUT}) 
    set(_verilator_cpp_flags ${gw_project_rules_VERILATOR_CPP_FLAGS})
    set(_verilator_ld_flags ${gw_project_rules_VERILATOR_LD_FLAGS})

    # Dummy output which is never actually produced. Anything that depends on
    # this will always be rebuilt.
    add_custom_command(
        OUTPUT always_rebuild
        COMMAND cmake -E echo
    )

    add_custom_command(
        OUTPUT
            ${_prj_name}.vlts
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_get_vlts.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.vlts
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
            ${_prj_name}.verilator_script
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_verilator_script.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.verilator_script ${_prj_name}
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS
            always_rebuild
        COMMENT
            "Generating verilator script."
        VERBATIM
    )

    add_custom_target(${_prj_name}_lint
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/verilator_lint_check.sh ${_top_module} ${_prj_name}.vlts ${_prj_name}.waiver ${_prj_name}.verilator_script ${_verilator_cpp_flags} ${_verilator_ld_flags}
        COMMAND_EXPAND_LISTS
        DEPENDS
            ${_prj_name}.vlts
            ${_prj_name}.verilator_script
        BYPRODUCTS 
            ${_prj_name}.waiver
        COMMENT
            "Lint check."
        VERBATIM
    )

    #Include lint target in the lint_all dependency list.
    add_dependencies(lint_all ${_prj_name}_lint)

    add_custom_command(
            OUTPUT
                ${_prj_name}.mem_file_script.tcl ${_mem_file_out}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/gen_mem_file_script.sh $<TARGET_FILE:${_mem_file_tgt}>.mem ${CMAKE_CURRENT_BINARY_DIR}/${_mem_file_out} ${_prj_name}.mem_file_script.tcl
            DEPENDS
                ${_mem_file_tgt}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating mem file script."
            VERBATIM
        )

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        add_custom_command(
            OUTPUT
                ${_prj_name}.vivado_script.tcl
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_vivado_script.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.vivado_script.tcl ${_prj_name}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                always_rebuild
            COMMENT
                "Generating vivado script."
            VERBATIM
        )

        add_custom_command(
            OUTPUT
                ${_prj_name}.constraints_script.tcl
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_constraints_script.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.constraints_script.tcl
            DEPENDS
                always_rebuild
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating constraints script."
            VERBATIM
        )

        add_custom_target(${_prj_name}_synth
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_cmd.sh synth ${PART} ${_prj_name}.vivado_script.tcl ${_prj_name}.constraints_script.tcl ${_prj_name}.mem_file_script.tcl ${CMAKE_CURRENT_BINARY_DIR} ${_top_module}
            DEPENDS
                ${_prj_name}.vivado_script.tcl 
                ${_prj_name}.constraints_script.tcl 
                ${_prj_name}.mem_file_script.tcl
            COMMENT
                "Project synthesis."
            VERBATIM
        )

        #A bunch of generated files that need to be cleaned up as part of make clean.
        set_target_properties(${_prj_name}_synth PROPERTIES ADDITIONAL_CLEAN_FILES 
        "project.cache;project.runs;project.xpr;project.ip_user_files;project.hw;syn_timing.rpt;syn_util.rpt")

        #Add the bitstream generation target to make all (ALL property).
        add_custom_target(${_prj_name}_bit
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_cmd.sh impl ${PART} ${_prj_name}.vivado_script.tcl ${_prj_name}.constraints_script.tcl ${_prj_name}.mem_file_script.tcl ${CMAKE_CURRENT_BINARY_DIR} ${_top_module}
            DEPENDS
                ${_prj_name}.vivado_script.tcl 
                ${_prj_name}.constraints_script.tcl 
                ${_prj_name}.mem_file_script.tcl
            COMMENT
                "Project implementation and bitstream generation."
            VERBATIM
        )

        #A bunch of generated files that need to be cleaned up as part of make clean.
        set_target_properties(${_prj_name}_bit PROPERTIES ADDITIONAL_CLEAN_FILES 
        "project.cache;project.runs;project.xpr;project.ip_user_files;project.hw;syn_timing.rpt;syn_util.rpt;imp_timing.rpt;imp_util.rpt")

        add_custom_target(${_prj_name}_load
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_load_bitstream.sh
            COMMENT
                "Loading bitstream to target."
            VERBATIM
        )

    else() #sim:
        add_custom_command(
            OUTPUT
                ${_prj_name}.cpp_files
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_get_cpp_files.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.cpp_files
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                always_rebuild
            COMMENT
                "Retrieving CPP files for Verilator build."
            VERBATIM
        )

        add_custom_command(
            OUTPUT
                Vmodel
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/verilator_sim.sh ${_prj_name}.verilator_script ${_prj_name}.vlts ${_prj_name}.cpp_files ${_top_module} ${_verilator_cpp_flags} ${_verilator_ld_flags} ${CMAKE_CURRENT_BINARY_DIR}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${_prj_name}.verilator_script 
                ${_prj_name}.vlts 
                ${_prj_name}.cpp_files
                ${_mem_file_out}
            COMMENT
                "Generating Verilator Vmodel."
            VERBATIM
        )

        add_custom_target(${_prj_name}_sim
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                Vmodel
            COMMENT
                "Simulation build."
            VERBATIM
        )

        set_target_properties(${_prj_name}_sim PROPERTIES ADDITIONAL_CLEAN_FILES ".")
    endif()    
endfunction()

add_subdirectory(hello_world)
add_subdirectory(hello_dbg)
add_subdirectory(picolibc_test)
add_subdirectory(ddr_test)
