#
# Forth Core Test Suite
#

add_executable(forth_core_test
 EXCLUDE_FROM_ALL
   testsuite.cpp
   testsuite.fs
)

target_compile_options(forth_core_test
 PRIVATE
  -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(forth_core_test ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)

target_link_libraries(forth_core_test forth_core timer gpio bootstrap)

add_flash_sw_target(forth_core_test)


