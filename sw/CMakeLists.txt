#The GCC specs file expects absolute paths. I'm using configure_file() to replace placeholders
#in picolibc.specs.in with PROJECT_SOURCE_DIR. The resulting picolibc.specs is written in the root of the build tree.
configure_file(${PROJECT_SOURCE_DIR}/scripts/picolibc.specs.in picolibc.specs @ONLY)

#Set the generated specs files as standard compile and link options.
set(SPECS "--specs=${CMAKE_CURRENT_BINARY_DIR}/picolibc.specs")

#Use the picolibc specs for all compilations.
add_compile_options(
    $<$<COMPILE_LANGUAGE:CXX,C,ASM>:${SPECS}>)

add_subdirectory(components)
add_subdirectory(projects)

