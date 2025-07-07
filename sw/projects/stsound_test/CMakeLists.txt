#
# ST Sound Test
#
add_executable(stsound_test
 EXCLUDE_FROM_ALL
 stsound_test.cpp
)

target_include_directories(stsound_test
    PRIVATE
      ${PROJECT_SOURCE_DIR}/registers/generated/
)

target_compile_options(stsound_test
 PRIVATE
  -O2 -g
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  target_compile_definitions(stsound_test PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
  target_compile_definitions(stsound_test PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

#Function defined in parent CMakeLists.txt file:
link_and_create_image(stsound_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(stsound_test sdram stsound fatfs gpio riscv bootstrap)

