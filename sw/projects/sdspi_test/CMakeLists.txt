#
# SDSPI Test
#
add_executable(sdspi_test
 EXCLUDE_FROM_ALL
 sdspi_test_main.cpp
 sdtest.cpp
)

target_compile_options(sdspi_test
 PRIVATE -g -O2)

target_compile_definitions(sdspi_test
 PRIVATE
  -DSDSPI_CPU_IS_LITTLE_ENDIAN
)

target_include_directories(sdspi_test
    PRIVATE
     ${PROJECT_SOURCE_DIR}/sw/components/sdspi/
     ${PROJECT_SOURCE_DIR}/registers/generated/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(sdspi_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(sdspi_test gpio riscv interrupts bootstrap)

