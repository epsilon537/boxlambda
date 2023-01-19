set(SPECS "--specs=${PROJECT_SOURCE_DIR}/sw/picolibc-install/picolibc.specs")

add_compile_options(${SPECS})
add_link_options(${SPECS} "LINKER:--gc-sections")

#Set link internal settings for given target and add byproduct command generating a .mem file.
function(link_internal_create_mem _tgt)
    target_link_libraries(${_tgt} bootstrap)

    target_link_options(${_tgt}
        PRIVATE
            -T${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem.ld
           "LINKER:--Map,${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.map"
    )

    set_target_properties(${_tgt} PROPERTIES LINK_DEPENDS ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem.ld)
    set_target_properties(${_tgt} PROPERTIES ADDITIONAL_CLEAN_FILES ${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.map)

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

add_subdirectory(components/bootstrap)
add_subdirectory(projects/hello_world)
add_subdirectory(projects/picolibc_test)
add_subdirectory(projects/ddr_test)

