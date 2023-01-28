#Function grouping gateware project build rules.
#TOP_MODULE: name of top module.
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
    set(_prj_comp_name ${gw_project_rules_PROJECT_NAME})
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
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_verilator_sources.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.verilator_sources ${CMAKE_CURRENT_BINARY_DIR}/Vmodel ${_prj_comp_name}
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
        COMMAND_EXPAND_LISTS
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

    add_custom_command(
            OUTPUT
                ${_prj_comp_name}.mem_file_list ${_mem_file_out}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/gen_mem_file_list.sh $<TARGET_FILE:${_mem_file_tgt}>.mem ${CMAKE_CURRENT_BINARY_DIR}/${_mem_file_out} ${_prj_comp_name}.mem_file_list
            DEPENDS
                ${_mem_file_tgt}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating mem file list."
            VERBATIM
        )

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        add_custom_command(
            OUTPUT
                ${_prj_comp_name}.vivado_sources
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
                ${_prj_comp_name}.constraints_file_list
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_constraints_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.constraints_file_list
            DEPENDS
                always_rebuild
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating constraints file list."
            VERBATIM
        )

        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.dep
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_create_project.sh ${PART} ${_prj_comp_name}.vivado_sources ${_prj_comp_name}.constraints_file_list ${_prj_comp_name}.mem_file_list ${CMAKE_CURRENT_BINARY_DIR} ${_top_module}
            BYPRODUCTS
                ${CMAKE_CURRENT_BINARY_DIR}/project.xpr
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${_prj_comp_name}.vivado_sources
                ${_prj_comp_name}.constraints_file_list
                ${_prj_comp_name}.mem_file_list
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
                "Project synthesis."
            VERBATIM
        )

        add_custom_target(${_prj_comp_name}_synth
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
            COMMENT
                "Project synthesis target."
        )

        #A bunch of generated files that need to be cleaned up as part of make clean.
        set_target_properties(${_prj_comp_name}_synth PROPERTIES ADDITIONAL_CLEAN_FILES 
        "project.cache;project.runs;project.xpr;project.ip_user_files;project.hw;syn_timing.rpt;syn_util.rpt")

        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/impl_1/${_top_module}.bit
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_impl.sh ${CMAKE_CURRENT_BINARY_DIR}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
            COMMENT
                "Project implementation."
            VERBATIM
        )

        #Add the bitstream generation target to make all (ALL property).
        add_custom_target(${_prj_comp_name}_impl
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/impl_1/${_top_module}.bit
            COMMENT
                "Project implementation target."
        )

        #A bunch of generated files that need to be cleaned up as part of make clean.
        set_target_properties(${_prj_comp_name}_impl PROPERTIES ADDITIONAL_CLEAN_FILES 
        "project.cache;project.runs;project.xpr;project.ip_user_files;project.hw;syn_timing.rpt;syn_util.rpt;imp_timing.rpt;imp_util.rpt")

        add_custom_target(${_prj_comp_name}_load
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
                ${_prj_comp_name}.cpp_files
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_get_cpp_files.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_comp_name}.cpp_files
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
                ${PROJECT_SOURCE_DIR}/scripts/verilator_sim.sh ${_prj_comp_name}.verilator_sources ${_prj_comp_name}.vlts ${_prj_comp_name}.cpp_files ${_top_module} ${_verilator_cpp_flags} ${_verilator_ld_flags} ${CMAKE_CURRENT_BINARY_DIR}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${_prj_comp_name}.verilator_sources 
                ${_prj_comp_name}.vlts 
                ${_prj_comp_name}.cpp_files
                ${_mem_file_out}
            DEPFILE
                ${_prj_comp_name}.verilator_sources.dep
            COMMENT
                "Generating Verilator Vmodel."
            VERBATIM
        )

        add_custom_target(${_prj_comp_name}_sim
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                Vmodel
            COMMENT
                "Simulation build."
            VERBATIM
        )

        set_target_properties(${_prj_comp_name}_sim PROPERTIES ADDITIONAL_CLEAN_FILES ".")
    endif()    
endfunction()

add_subdirectory(hello_world)
add_subdirectory(hello_dbg)
add_subdirectory(picolibc_test)
add_subdirectory(ddr_test)
