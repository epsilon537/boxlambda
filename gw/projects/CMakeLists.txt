#Function grouping gateware project build rules for Vivado builds.
#TOP_MODULE: name of top module, e.g. ibex_soc
#PROJECT_NAME: project name, e.g. hello_dbg
#MEM_FILE_TARGET: name of the SW target that generates the required .mem file, e.g. picolibc_test
#MEM_FILE_LIST: Mem file list, if any.

function (gw_project_rules_vivado)
  set(options)
  set(oneValueArgs TOP_MODULE PROJECT_NAME MEM_FILE_TARGET MEM_FILE_LIST )
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
  set(_mem_file_list ${gw_common_rules_MEM_FILE_LIST})

  if (BL_TARGET_FPGA STREQUAL "arty-a7-100")
    set(_board arty_a7_100t)
  else()
    set(_board arty_a7_35t)
  endif()

  add_custom_command(
        OUTPUT
            ${_prj_name}.constraints_file_list
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_gen_constraints_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.constraints_file_list ${BL_TARGET_FPGA}
        DEPENDS
            always_rebuild
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT
            "Generating constraints file list."
        VERBATIM
    )

  #Build rules, shared with gateware components : lint target, Vivado synthesis
  gw_common_rules(
        TOP_MODULE
            ${_top_module}
        PRJ_COMP_NAME
            ${_prj_name}
        MEM_FILE_LIST
            ${_prj_name}.mem_file_list
        CONSTRAINTS_FILE_LIST
            ${_prj_name}.constraints_file_list
    )

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
            "Implementing project and generating bitstream."
        VERBATIM
    )

  #Custom command converting the bitstream file into a .bin file ready for flashing.
  add_custom_command(
        OUTPUT
            ./project.runs/impl_1/${_top_module}.bit.bin
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bit2bin.sh ./project.runs/impl_1/${_top_module}.bit
        DEPENDS
            ./project.runs/impl_1/${_top_module}.bit
        COMMENT
            "Converting bitstream to .bin."
        VERBATIM
    )

  #Add the bitstream generation target to make all (ALL property).
  add_custom_target(${_prj_name}_bit
        ALL
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS
            ${CMAKE_CURRENT_BINARY_DIR}/project.runs/impl_1/${_top_module}.bit.bin
        COMMENT
            "Project implementation and bitstream generation target."
    )

  if (NOT ${_mem_file_tgt} STREQUAL "")
    #Custom command updating the SW memory file in the bitstream.
    add_custom_command(
            OUTPUT
                ./project.runs/impl_1/${_top_module}.sw.bit
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/vivado_updatemem.sh ./project.runs/impl_1/${_top_module}.mmi $<TARGET_FILE:${_mem_file_tgt}>.mem ./project.runs/impl_1/${_top_module}.bit ./project.runs/impl_1/${_top_module}.sw.bit
            DEPENDS
                ${_mem_file_tgt}
                ${_prj_name}_bit
            COMMENT
                "Updating SW memory image in bitstream."
            VERBATIM
        )

    #Custom command converting the bitstream file into a .bin file ready for flashing.
    add_custom_command(
            OUTPUT
                ./project.runs/impl_1/${_top_module}.sw.bit.bin
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                ${PROJECT_SOURCE_DIR}/scripts/bit2bin.sh ./project.runs/impl_1/${_top_module}.sw.bit
            DEPENDS
                ./project.runs/impl_1/${_top_module}.sw.bit
            COMMENT
                "Converting bitstream to .bin."
            VERBATIM
        )

    #Add the bitstream-with-sw-image target
    add_custom_target(${_prj_name}_bit_sw
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                ./project.runs/impl_1/${_top_module}.sw.bit.bin
            COMMENT
                "Bitstream + SW image target."
        )
    set(_bit_file ./project.runs/impl_1/${_top_module}.sw.bit)
  else()
    set(_bit_file ./project.runs/impl_1/${_top_module}.bit)
  endif()

  #Custom command executing the vivado prg_bitstream command string.
  add_custom_target(${_prj_name}_load
          WORKING_DIRECTORY
              ${CMAKE_CURRENT_BINARY_DIR}
          COMMAND
              ${PROJECT_SOURCE_DIR}/scripts/load_gw.sh ${_board} ${_bit_file}
          COMMENT
              "Loading bitstream to target."
          VERBATIM
      )

  #Custom command executing the vivado flash_bitstream command string.
  add_custom_target(${_prj_name}_flash_gw
          WORKING_DIRECTORY
              ${CMAKE_CURRENT_BINARY_DIR}
          COMMAND
              ${PROJECT_SOURCE_DIR}/scripts/flash_gw.sh ${_board} ${_bit_file}.bin
          COMMENT
              "Flashing bitstream to target."
          VERBATIM
      )
endfunction()

#Function grouping gateware project build rules for Verilator builds.
#TOP_MODULE: name of top module, e.g. ibex_soc
#PROJECT_NAME: project name, e.g. hello_dbg
#MEM_FILE_TARGET: name of the SW target that generates the required .mem file, e.g. picolibc_test
#MEM_FILE_LIST: Mem file list, if any.
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty.
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty.
function (gw_project_rules_verilator)
  set(options)
  set(oneValueArgs TOP_MODULE PROJECT_NAME MEM_FILE_TARGET MEM_FILE_LIST VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
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
  set(_mem_file_list ${gw_common_rules_MEM_FILE_LIST})
  set(_verilator_cpp_flags ${gw_project_rules_VERILATOR_CPP_FLAGS})
  set(_verilator_ld_flags ${gw_project_rules_VERILATOR_LD_FLAGS})

  #Build rules, shared with gateware components : lint target, Vivado synthesis
  gw_common_rules(
        TOP_MODULE
            ${_top_module}
        PRJ_COMP_NAME
            ${_prj_name}
        MEM_FILE_LIST
            ${_prj_name}.mem_file_list
        VERILATOR_CPP_FLAGS
            ${_verilator_cpp_flags}
        VERILATOR_LD_FLAGS
            ${_verilator_ld_flags}
    )

  add_custom_command(
        OUTPUT
            ${_prj_name}.cpp_files
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/bender_get_cpp_files.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.cpp_files ${BL_TARGET_FPGA}
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
            "Verilator build target."
        VERBATIM
    )

  if (NOT ${_mem_file_tgt} STREQUAL "")
    #Custom command copying the SW image to its proper place in the gateware build directory
    add_custom_command(
            OUTPUT
                cmem.mem
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND
                cp $<TARGET_FILE:${_mem_file_tgt}>.mem ${CMAKE_CURRENT_BINARY_DIR}/cmem.mem
            DEPENDS
                ${_mem_file_tgt}
            COMMENT
                "Copying SW memory image to gateware build dir."
            VERBATIM
        )

    #Verilator model + SW image target
    add_custom_target(${_prj_name}_sim_sw
            ALL
            WORKING_DIRECTORY
                ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS
                cmem.mem
                ${_prj_name}_sim
            COMMENT
                "Verilator Vmodel + SW image target."
        )
  endif()
endfunction()

#Function grouping gateware project build rules.
#TOP_MODULE: name of top module, e.g. ibex_soc
#PROJECT_NAME: project name, e.g. hello_dbg
#MEM_FILE_TARGET: name of the SW target that generates the required .mem file, e.g. picolibc_test
#VERILATOR_CPP_FLAGS: CPP flags to use when verilating. Usually empty.
#VERILATOR_LD_FLAGS: LD flags to use when verilating. Usually empty.
function (gw_project_rules)
  set(options)
  set(oneValueArgs TOP_MODULE PROJECT_NAME MEM_FILE_TARGET VERILATOR_CPP_FLAGS VERILATOR_LD_FLAGS)
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
  set(_verilator_cpp_flags ${gw_project_rules_VERILATOR_CPP_FLAGS})
  set(_verilator_ld_flags ${gw_project_rules_VERILATOR_LD_FLAGS})

  add_custom_command(
        OUTPUT
            ${_prj_name}.mem_file_list
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/gen_mem_file_list.sh ${CMAKE_CURRENT_LIST_DIR} ${_prj_name}.mem_file_list
        DEPENDS
            always_rebuild
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT
            "Generating mem file list."
        VERBATIM
    )

  if(CMAKE_BUILD_TYPE STREQUAL "fpga")
    gw_project_rules_vivado(
            TOP_MODULE
                ${_top_module}
            PROJECT_NAME
                ${_prj_name}
            MEM_FILE_TARGET
                ${_mem_file_tgt}
            MEM_FILE_LIST
                ${_prj_name}.mem_file_list
        )
  else()
    gw_project_rules_verilator(
            TOP_MODULE
                ${_top_module}
            PROJECT_NAME
                ${_prj_name}
            MEM_FILE_TARGET
                ${_mem_file_tgt}
            MEM_FILE_LIST
                ${_prj_name}.mem_file_list
            VERILATOR_CPP_FLAGS
                ${_verilator_cpp_flags}
            VERILATOR_LD_FLAGS
                ${_verilator_ld_flags}
        )
  endif()
endfunction()

add_subdirectory(hello_world)
add_subdirectory(ddr_test)
add_subdirectory(vera_integrated)
add_subdirectory(sdspi_test)
add_subdirectory(fatfs_test)
add_subdirectory(audio_dac_test)
add_subdirectory(ym2149_dac_test)
if (BL_TARGET_FPGA STREQUAL "arty-a7-100")
  add_subdirectory(stsound_test)
endif()
add_subdirectory(picorv_dma_sys_test_ext)
add_subdirectory(dual_port_ram_test)
add_subdirectory(usb_hid_sys_test)
add_subdirectory(spiflash_test)
add_subdirectory(gpios_timers_and_irqs)

