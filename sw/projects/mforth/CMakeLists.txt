#
# Mecrisp Quintus Forth
#
add_executable(mforth
 EXCLUDE_FROM_ALL
 main.cpp
 mecrisp-quintus-boxlambda.S
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
