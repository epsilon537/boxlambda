set(SPECS "--specs=${PROJECT_SOURCE_DIR}/sw/picolibc-install/picolibc.specs")

add_compile_options(${SPECS})
add_link_options(${SPECS} "LINKER:--gc-sections")

add_subdirectory(components)
add_subdirectory(projects)

