#Flash test program - Flash Build

add_executable(spiflash_test_flsh
 EXCLUDE_FROM_ALL
 spiflash_test.cpp
)

target_compile_options(spiflash_test_flsh
 PRIVATE
  -O2 -g
)

#spiflash_test_ram is linked as Flash memory image.
link_and_create_image(spiflash_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(spiflash_test_flsh flashdrvr sdram gpio riscv bootstrap)

add_flash_sw_target(spiflash_test_flsh)

#RAM build variant:

add_executable(spiflash_test_ram
 EXCLUDE_FROM_ALL
 spiflash_test.cpp
)

target_compile_options(spiflash_test_ram
 PRIVATE
  -O2 -g
)

#spiflash_test_ram is linked as IMEM memory image.
link_and_create_image(spiflash_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(spiflash_test_ram flashdrvr sdram gpio riscv bootstrap)

