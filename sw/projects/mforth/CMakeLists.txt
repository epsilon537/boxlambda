#
# BoxLambda port of Mecrisp Quintus Forth
#

# Hack: rebuild mecrisp-boxlambda.S when one of the .included .s files change.
file(GLOB FORTH_S_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.s")

add_custom_command(
    OUTPUT stamp
    DEPENDS ${FORTH_S_SOURCES}
    COMMAND ${CMAKE_COMMAND} -E touch stamp ${CMAKE_CURRENT_SOURCE_DIR}/mecrisp-quintus-boxlambda.S
    COMMENT "Touching mecrisp-quintus.boxlambda.S because .s files changed."
)
set_source_files_properties(
  ${CMAKE_CURRENT_SOURCE_DIR}/mecrisp-quintus-boxlambda.S
  PROPERTIES SYMBOLIC TRUE)

add_executable(mforth
 EXCLUDE_FROM_ALL
   main.cpp
   forth.cpp
   mecrisp-quintus-boxlambda.S
   stamp
)

target_compile_options(mforth
 PRIVATE
  -O2 -g
)

target_compile_definitions(mforth
 PRIVATE
  -DBOARD_BOXLAMBDA
)

target_include_directories(mforth
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated
  ${CMAKE_CURRENT_SOURCE_DIR}
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(mforth ${CMAKE_CURRENT_SOURCE_DIR}/link_mforth.ld)

target_link_libraries(mforth gpio bootstrap)

add_flash_sw_target(mforth)

