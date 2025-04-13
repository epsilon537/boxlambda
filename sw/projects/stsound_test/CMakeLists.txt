#
# ST Sound Test - RAM Build
#
add_executable(stsound_test_ram
 EXCLUDE_FROM_ALL
 stsound_test.cpp
)

target_include_directories(stsound_test_ram
    PRIVATE
 ${PROJECT_SOURCE_DIR}/sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/
 ${PROJECT_SOURCE_DIR}/sw/components/ym2149_sys_hal/
)

target_compile_options(stsound_test_ram
 PRIVATE
  -O2 -g
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  target_compile_definitions(stsound_test_ram PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
  target_compile_definitions(stsound_test_ram PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

#Function defined in parent CMakeLists.txt file:
link_and_create_image(stsound_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(stsound_test_ram sdram stsound fatfs gpio riscv bootstrap)

#
# ST Sound Test - Flash Build
#
add_executable(stsound_test_flsh
 EXCLUDE_FROM_ALL
 stsound_test.cpp
)

target_include_directories(stsound_test_flsh
    PRIVATE
     ${PROJECT_SOURCE_DIR}/sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/
     ${PROJECT_SOURCE_DIR}/sw/components/ym2149_sys_hal/
)

target_compile_options(stsound_test_flsh
 PRIVATE
  -O2 -g
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  target_compile_definitions(stsound_test_flsh PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
  target_compile_definitions(stsound_test_flsh PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

#Function defined in parent CMakeLists.txt file:
link_and_create_image(stsound_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(stsound_test_flsh sdram stsound fatfs gpio riscv bootstrap)

add_flash_sw_target(stsound_test_flsh)
