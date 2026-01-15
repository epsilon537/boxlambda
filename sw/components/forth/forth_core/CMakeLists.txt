# The Mecrisp Quintus Forth Core + C FFI
add_library(forth_core)

# Rebuild mecrisp-boxlambda.s when one of the .included .s files change.
file(GLOB FORTH_S_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.s")
# Remove mecrisp-quintus-boxlambda.s from the FORTH_S_SOURCES list. That's
# the file that does the .including.
list(REMOVE_ITEM FORTH_S_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/mecrisp-quintus-boxlambda.s)

# For some obscure CMake reason, touching the stamp fake dependency is not sufficient.
# Both stamp and mecrisp-quintus-boxlambda.s need to be touched.
add_custom_command(
  OUTPUT stamp
  DEPENDS ${FORTH_S_SOURCES}
  COMMAND ${CMAKE_COMMAND} -E touch stamp ${CMAKE_CURRENT_SOURCE_DIR}/mecrisp-quintus-boxlambda.s
  COMMENT "Touching mecrisp-quintus-boxlambda.s file because .included .s files changed."
)

# The dummy stamp file has to be added as a dependency but won't get compiled.
target_sources(forth_core
  PRIVATE
    mecrisp-quintus-boxlambda.s
    forth.cpp
    init.fs
    stamp
)

target_compile_options(forth_core
  PRIVATE
    -O2 -g -ffunction-sections
)

target_compile_definitions(forth_core
  PRIVATE
    -DBOARD_BOXLAMBDA
)

target_include_directories(forth_core
  PUBLIC
    ${PROJECT_SOURCE_DIR}/registers/generated
    ${CMAKE_CURRENT_SOURCE_DIR}
)

