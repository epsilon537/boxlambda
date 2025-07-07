#
# I2C Test
#
add_executable(i2c_test
 EXCLUDE_FROM_ALL
 i2c_test.cpp
 i2c_cli.cpp
)

target_compile_options(i2c_test
 PRIVATE
  -O2 -g -ffunction-sections
)

link_and_create_image(i2c_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(i2c_test sdram i2c gpio embedded_cli riscv interrupts peek_poke_cli bootstrap)

