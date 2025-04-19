#Build PicoLibc during the build tree configuration step.
execute_process(
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/picolibc_build.sh ${PROJECT_SOURCE_DIR}/sub/picolibc ${CMAKE_CURRENT_BINARY_DIR}/picolibc-build ${CMAKE_CURRENT_BINARY_DIR}/picolibc-install
)

#Set the generated specs files as standard compile and link options.
set(SPECS "--specs=${CMAKE_CURRENT_BINARY_DIR}/picolibc-install/picolibc.specs")

#Use the picolibc specs for all compilations.
add_compile_options(
    $<$<COMPILE_LANGUAGE:CXX,C,ASM>:${SPECS}>)

add_link_options(
    $<$<COMPILE_LANGUAGE:CXX,C,ASM>:${SPECS}>)

add_subdirectory(components)
add_subdirectory(projects)

