#
# SDSPI Test RAM Build
#
add_executable(sdspi_test_ram
 EXCLUDE_FROM_ALL
 sdspi_test_main.c
 sdtest.c
)

target_compile_options(sdspi_test_ram
 PRIVATE -g -O2)

target_compile_definitions(sdspi_test_ram
 PRIVATE
  -DSDSPI_CPU_IS_LITTLE_ENDIAN
)

target_include_directories(sdspi_test_ram
    PRIVATE
     ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(sdspi_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(sdspi_test_ram gpio riscv interrupts bootstrap)

#
# SDSPI Test Flash Build
#
add_executable(sdspi_test_flsh
 EXCLUDE_FROM_ALL
 sdspi_test_main.c
 sdtest.c
)

target_compile_options(sdspi_test_flsh
 PRIVATE -g -O2)

target_compile_definitions(sdspi_test_flsh
 PRIVATE
  -DSDSPI_CPU_IS_LITTLE_ENDIAN
)

target_include_directories(sdspi_test_flsh
    PRIVATE
     ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(sdspi_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(sdspi_test_flsh gpio riscv interrupts bootstrap)

add_flash_sw_target(sdspi_test_flsh)
