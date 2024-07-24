
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

link_and_create_mem_file(rtcc_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(rtcc_test_ram sdram rtcc i2c gpio riscv embedded_cli peek_poke_cli)

target_include_directories(rtcc_test_ram
    PUBLIC
       ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/
)

add_executable(rtcc_test
 EXCLUDE_FROM_ALL
 rtcc_test.cpp
 rtcc_cli.cpp
 ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/i2c_cli.cpp
)

target_compile_options(rtcc_test
 PRIVATE
  -O2 -g
)

link_and_create_mem_file(rtcc_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(rtcc_test sdram rtcc i2c gpio riscv embedded_cli peek_poke_cli)

target_include_directories(rtcc_test
    PUBLIC
       ${PROJECT_SOURCE_DIR}/sw/projects/i2c_test/
)

add_flash_sw_target(rtcc_test)

