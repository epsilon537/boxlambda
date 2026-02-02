#
# Forth Core Test Suite
#

add_executable(forth_core_test
   testsuite.cpp
   testsuite.fs
)

target_compile_options(forth_core_test
 PRIVATE
  -O2 -g
)

target_include_directories(forth_core_test
  PRIVATE
   ${PROJECT_SOURCE_DIR}/sw/components/forth/forth
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(forth_core_test ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)

target_link_libraries(forth_core_test fatfs forth_core forth_fs timer gpio bootstrap)

add_flash_sw_target(forth_core_test)

if(CMAKE_BUILD_TYPE STREQUAL "fpga")
#Automated test
add_test(NAME forth_core_test_test
    COMMAND python3 forth_core_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/test
)
endif()
