#
# RTCC Test RAM Build
#
add_executable(rtcc_test_ram
 EXCLUDE_FROM_ALL
 rtcc_test.cpp
 rtcc_cli.cpp
 ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/i2c_cli.cpp
)

target_compile_options(rtcc_test_ram
 PRIVATE
  -O2 -g
)

link_and_create_image(rtcc_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(rtcc_test_ram sdram rtcc i2c gpio riscv embedded_cli peek_poke_cli)

target_include_directories(rtcc_test_ram
    PUBLIC
       ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/
)

#
# RTCC Test Flash Build
#
add_executable(rtcc_test_flsh
 EXCLUDE_FROM_ALL
 rtcc_test.cpp
 rtcc_cli.cpp
 ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/i2c_cli.cpp
)

target_compile_options(rtcc_test_flsh
 PRIVATE
  -O2 -g
)

link_and_create_image(rtcc_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(rtcc_test_flsh sdram rtcc i2c gpio riscv embedded_cli peek_poke_cli)

target_include_directories(rtcc_test_flsh
    PUBLIC
       ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/
)

add_flash_sw_target(rtcc_test_flsh)

