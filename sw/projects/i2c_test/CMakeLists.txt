#
# I2C Test RAM Build
#
add_executable(i2c_test_ram
 EXCLUDE_FROM_ALL
 i2c_test.cpp
 i2c_cli.cpp
)

target_compile_options(i2c_test_ram
 PRIVATE
  -O2 -g -ffunction-sections
)

link_and_create_image(i2c_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(i2c_test_ram sdram i2c gpio embedded_cli riscv interrupts peek_poke_cli bootstrap)

#
# I2C Test Flash Build
#
add_executable(i2c_test_flsh
 EXCLUDE_FROM_ALL
 i2c_test.cpp
 i2c_cli.cpp
)

target_compile_options(i2c_test_flsh
 PRIVATE
  -O2 -g -ffunction-sections
)

link_and_create_image(i2c_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(i2c_test_flsh sdram i2c gpio embedded_cli riscv interrupts peek_poke_cli bootstrap)

add_flash_sw_target(i2c_test_flsh)
