#Flash test program

add_executable(spiflash_test
 EXCLUDE_FROM_ALL
 spiflash_test.cpp
)

target_compile_options(spiflash_test
 PRIVATE
  -O2 -g
)

link_and_create_image(spiflash_test
${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)

target_link_libraries(spiflash_test flashdrvr sdram gpio riscv bootstrap)

