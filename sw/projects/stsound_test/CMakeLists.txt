add_executable(stsound_test
 EXCLUDE_FROM_ALL
 stsound_test.cpp
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

target_include_directories(stsound_test
    PRIVATE
 ${PROJECT_SOURCE_DIR}/sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/
 ${PROJECT_SOURCE_DIR}/sw/components/ym2149_sys_hal/
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
link_and_create_mem_file(stsound_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(stsound_test sdram stsound fatfs)
