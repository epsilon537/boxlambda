#Function adding a *_flash_sw target/rule for SW project builds:

function(add_flash_sw_target _tgt)
  set(_board arty_a7_100t)

  add_custom_target(${_tgt}_load_sw
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/target.py -load_app ${_tgt}.bin -run
        COMMENT
            "Loading SW to target."
        VERBATIM
  )

  add_custom_target(${_tgt}_flash_sw
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND
            ${PROJECT_SOURCE_DIR}/scripts/target.py -flash_app ${_tgt}.bin
        COMMENT
            "Flashing SW to target."
        VERBATIM
  )
endfunction()

#Link given target using given linker script and add byproduct command generating a .mem file.
function(link_and_create_image _tgt _link_script)
  target_link_options(${_tgt}
        PRIVATE
            # Don't use Picolibc's crt0. We provide our own crt0 in
            # sw/components/bootstrap
            -nostartfiles
            -T${_link_script}
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

  #Add it to the sw target dependency list
  add_dependencies(sw ${_tgt})

endfunction()

#SW project builds:
add_subdirectory(bootloader)
add_subdirectory(boxlambda_os)
add_subdirectory(test)


