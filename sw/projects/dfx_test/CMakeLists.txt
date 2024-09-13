#DFX test program

add_executable(dfx_test
 EXCLUDE_FROM_ALL
 dfx_test.cpp
 dfx_cli.cpp
 j1b_cli.cpp
)

target_compile_options(dfx_test
 PRIVATE
  -O2 -g
)

target_include_directories(dfx_test
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/reset/
  ${PROJECT_SOURCE_DIR}/sw/components/dfx/
)

#spiflash_test is linked as a flash memory image.
link_and_create_mem_file(dfx_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(dfx_test sdram gpio embedded_cli riscv interrupts peek_poke_cli j1b_hal fatfs ymodem_cli mem_fs_cli)

add_flash_sw_target(dfx_test)

