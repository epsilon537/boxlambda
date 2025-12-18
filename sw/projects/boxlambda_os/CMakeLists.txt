#
# BoxLambda Operating System (WIP)
#

add_executable(boxlambda_os
   main.cpp
)

target_compile_options(boxlambda_os
 PRIVATE
  -O2 -g
)

# target_include_directories(boxlambda_os
#   PRIVATE
#   ${PROJECT_SOURCE_DIR}/sw/components/arduino
#   ${PROJECT_SOURCE_DIR}/registers/generated
# )

#Function defined in parent CMakeLists.txt file:
link_and_create_image(boxlambda_os ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)

target_link_libraries(boxlambda_os forth_core gpio bootstrap)

add_flash_sw_target(boxlambda_os)

