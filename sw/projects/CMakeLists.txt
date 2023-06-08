#Function for SW project builds:
#Set link-for-internal-memory settings for given target and add byproduct command generating a .mem file.
function(link_internal_create_mem_file _tgt _link_script)
    target_link_libraries(${_tgt} bootstrap)

    #Link-for-internal-memory implies linking in the bootstrap object library.
    target_link_options(${_tgt}
        PRIVATE
            -T${_link_script}
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
            ${PROJECT_SOURCE_DIR}/sub/ibex_wb/scripts/hex2vmem.pl < ${_tgt}.hex > ${_tgt}.mem
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        BYPRODUCTS 
            ${_tgt}.hex ${_tgt}.mem
        COMMENT
            "Converting executable to mem."
        VERBATIM
    )
endfunction()

#SW project builds:
add_subdirectory(hello_world)
add_subdirectory(picolibc_test)
add_subdirectory(ddr_test)
add_subdirectory(vera_test)
add_subdirectory(sdspi_test)
add_subdirectory(fatfs_test)
add_subdirectory(ym2149_test)
add_subdirectory(stsound_test)
