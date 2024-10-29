#The GCC specs file expects absolute paths. I'm using configure_file() to replace placeholders
#in picolibc.specs.in with PROJECT_SOURCE_DIR. The resulting picolibc.specs is written in the root of the build tree.
configure_file(${PROJECT_SOURCE_DIR}/scripts/picolibc.specs.in picolibc.specs @ONLY)

#Set the generated specs files as standard compile and link options.
set(SPECS "--specs=${CMAKE_CURRENT_BINARY_DIR}/picolibc.specs")

#Use the picolibc specs for all compilations except picorv assembly.
add_compile_options(
    $<$<COMPILE_LANGUAGE:CXX,C,ASM>:${SPECS}>)

#Function for picorv32 builds (picorv_dma microcode) to link and generate binary
function(link_gen_picobin _tgt)
  #Text section should start at 0x10002000.
  target_link_options(${_tgt}
        PRIVATE
            -Ttext 0x10002000)

  #Binary file generation command.
  #Also generates C header file containing generated binary as a C array.
  add_custom_command(
        TARGET
            ${_tgt}
        POST_BUILD
        COMMAND
            ${CMAKE_OBJCOPY} -O binary ${_tgt} ${_tgt}.picobin
        COMMAND
            xxd -i ${_tgt}.picobin > ${_tgt}.h
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        BYPRODUCTS
            ${_tgt}.picobin
            ${_tgt}.h
        COMMENT
            "Converting picorv32 executable to binary and header file."
        VERBATIM
    )
endfunction()

function(build_picoasm _tgt _src)
  add_custom_command(
        OUTPUT
            ${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.picoasm
        COMMAND
            ${TOOLCHAIN_PREFIX}cpp -o ${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.picoasm ${_src}
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS
            ${_src}
    )

  add_executable(${_tgt}
     EXCLUDE_FROM_ALL
        ${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.picoasm
    )

  link_gen_picobin(${_tgt})
endfunction()

add_subdirectory(components)
add_subdirectory(projects)

