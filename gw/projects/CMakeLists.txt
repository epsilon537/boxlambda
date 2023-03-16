#Function grouping gateware project build rules.
#TOP_MODULE: name of top module.
#PROJECT_NAME: project name, e.g. hello_dbg
#MEM_FILE_TARGET: name of the SW target that generates the required .mem file, e.g. picolibc_test
#MEM_FILE_OUT: .mem file created by this function, and expected by the project build, e.g. spram.mem
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty. 
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty. 
function (gw_project_rules)
    set(options)
    set(oneValueArgs TOP_MODULE PROJECT_NAME MEM_FILE_TARGET MEM_FILE_OUT VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
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

    if (NOT ${_mem_file_tgt} STREQUAL "")
        add_custom_command(
            OUTPUT
                ${_prj_name}.mem_file_list ${_mem_file_out}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/gen_mem_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.mem_file_list $<TARGET_FILE:${_mem_file_tgt}>.mem ${CMAKE_CURRENT_BINARY_DIR}/${_mem_file_out}
            DEPENDS
                ${_mem_file_tgt}
                always_rebuild
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating mem file list."
            VERBATIM
        )
    else()
        add_custom_command(
            OUTPUT
                ${_prj_name}.mem_file_list
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/gen_mem_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.mem_file_list
            DEPENDS
                ${_mem_file_tgt}
                always_rebuild
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating mem file list."
            VERBATIM
        )
    endif()

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        add_custom_command(
            OUTPUT
                ${_prj_name}.constraints_file_list
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bender_gen_constraints_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.constraints_file_list
            DEPENDS
                always_rebuild
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT
                "Generating constraints file list."
            VERBATIM
        )
    endif()

    #Build rules, shared with gateware components : lint target, Vivado synthesis
    gw_common_rules(
        TOP_MODULE
            ${_top_module}
        PRJ_COMP_NAME
            ${_prj_name}
        CONSTRAINTS_FILE_LIST
            ${_prj_name}.constraints_file_list
        MEM_FILE_LIST
            ${_prj_name}.mem_file_list
        VERILATOR_CPP_FLAGS
            ${_verilator_cpp_flags}
        VERILATOR_LD_FLAGS
            ${_verilator_ld_flags}
    )

    if(CMAKE_BUILD_TYPE STREQUAL "fpga")
        #Build Vivado command string for the vivado_impl tcl script.
        vivado_cmd(impl_cmd ${PROJECT_SOURCE_DIR}/scripts/vivado_impl.tcl
            -project project.xpr -outputDir $OUTDIR ${CMAKE_CURRENT_BINARY_DIR})

        #Custom command executing the vivado implementation command string.
        add_custom_command(
            OUTPUT
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/impl_1/${_top_module}.bit
            COMMAND
                bash -c ${impl_cmd}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/synth_1/${_top_module}.dcp
            COMMENT
                "Project implementation."
            VERBATIM
        )

        #Add the bitstream generation target to make all (ALL property).
        add_custom_target(${_prj_name}_impl
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${CMAKE_CURRENT_BINARY_DIR}/project.runs/impl_1/${_top_module}.bit
            COMMENT
                "Project implementation target."
        )

        #Build Vivado command string for the prg_bitstream tcl script.
        vivado_cmd(load_cmd ${PROJECT_SOURCE_DIR}/scripts/prg_bitstream.tcl
            -bitstream ./project.runs/impl_1/*.bit)

        #Custom command executing the vivado prg_bitstream command string.
        add_custom_target(${_prj_name}_load
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                bash -c ${load_cmd}
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
                ${PROJECT_SOURCE_DIR}/scripts/verilator_sim.sh ${_prj_name}.verilator_sources ${_prj_name}.vlts ${_prj_name}.cpp_files ${_top_module} ${_verilator_cpp_flags} ${_verilator_ld_flags} ${CMAKE_CURRENT_BINARY_DIR}
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ${_prj_name}.mem_file_list
                ${_prj_name}.verilator_sources 
                ${_prj_name}.vlts 
                ${_prj_name}.cpp_files
                ${_mem_file_out}
            DEPFILE
                ${_prj_name}.verilator_sources.dep
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

    endif()    
endfunction()

add_subdirectory(hello_world)
add_subdirectory(hello_dbg)
add_subdirectory(picolibc_test)
add_subdirectory(ddr_test)
if(CMAKE_BUILD_TYPE STREQUAL "sim")
    add_subdirectory(vera_standalone)
endif()
add_subdirectory(vera_integrated)