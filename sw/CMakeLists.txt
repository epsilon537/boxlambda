configure_file(${PROJECT_SOURCE_DIR}/scripts/picolibc.specs.in picolibc.specs @ONLY)

set(SPECS "--specs=${CMAKE_CURRENT_BINARY_DIR}/picolibc.specs")

add_compile_options(${SPECS})
add_link_options(${SPECS} "LINKER:--gc-sections")

add_subdirectory(components)
add_subdirectory(projects)

