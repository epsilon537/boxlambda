#Flash test program

add_executable(spiflash_test
 EXCLUDE_FROM_ALL
 spiflash_test.cpp
)

target_compile_options(spiflash_test
 PRIVATE
  -O2 -g
)

target_include_directories(spiflash_test
    PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/platform/
)

#spiflash_test is linked as a flash memory image.
link_and_create_mem_file(spiflash_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(spiflash_test flashdrvr sdram gpio mcycle)

add_flash_sw_target(spiflash_test)

#RAM build variant:

add_executable(spiflash_test_ram
 EXCLUDE_FROM_ALL
 spiflash_test.cpp
)

target_compile_options(spiflash_test_ram
 PRIVATE
  -O2 -g
)

#spiflash_test_ram is linked as CMEM memory image.
link_and_create_mem_file(spiflash_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(spiflash_test_ram flashdrvr sdram gpio mcycle)

