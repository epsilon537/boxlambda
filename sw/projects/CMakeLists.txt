#Function for SW project builds:

function(add_flash_sw_target _tgt)
  if (BL_TARGET_FPGA STREQUAL "arty-a7-100")
    set(_board arty_a7_100t)
  else()
    set(_board arty_a7_35t)
  endif()

  add_custom_target(${_tgt}_flash_sw
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/flash_sw.sh ${_board} ${_tgt}.bin
        COMMENT
            "Flashing SW to target."
        VERBATIM
  )
endfunction()

#Set link-for-internal-memory settings for given target and add byproduct command generating a .mem file.
function(link_and_create_mem_file _tgt _link_script)
  target_link_libraries(${_tgt} bootstrap)

  #Link-for-internal-memory implies linking in the bootstrap object library.
  target_link_options(${_tgt}
        PRIVATE
            -T${_link_script}
            "--specs=${CMAKE_BINARY_DIR}/sw/picolibc.specs"
            "LINKER:--defsym=__cmem_size=${CMEM_SIZE}"
            "LINKER:--defsym=__dmem_size=${DMEM_SIZE}"
            "LINKER:--gc-sections"
            "LINKER:--Map,${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.map"
    )

  #.mem file generation commands.
  add_custom_command(
        TARGET
            ${_tgt}
        POST_BUILD
        COMMAND
            ${CMAKE_OBJCOPY} -O verilog --interleave-width=4 --interleave=4 --byte=0 ${_tgt} ${_tgt}.hex
        COMMAND
            ${CMAKE_OBJCOPY} -O binary ${_tgt} ${_tgt}.bin
        COMMAND
            ${PROJECT_SOURCE_DIR}/sub/ibex_wb/scripts/hex2vmem.pl < ${_tgt}.hex > ${_tgt}.mem
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        BYPRODUCTS
            ${_tgt}.hex ${_tgt}.mem ${_tgt}.bin
        COMMENT
            "Converting executable to mem and bin."
        VERBATIM
    )
endfunction()

#SW project builds:
add_subdirectory(hello_world)
add_subdirectory(ddr_test)
add_subdirectory(vera_test)
add_subdirectory(sdspi_test)
add_subdirectory(fatfs_test)
add_subdirectory(ym2149_test)
add_subdirectory(stsound_test)
add_subdirectory(picorv_dma_sys_test_ext)
add_subdirectory(dual_port_ram_test)
add_subdirectory(usb_hid_sys_test)
add_subdirectory(spiflash_test)
add_subdirectory(cmem_to_flash_vector)
add_subdirectory(timer_uart_gpio_irqs)
add_subdirectory(i2c_test)
add_subdirectory(rtcc_test)
add_subdirectory(reset_test)
add_subdirectory(j1b_test)
add_subdirectory(dfx_test)

