# J1B test program to be run on top off the j1b_test gateware build. I created
# this test in preparation of dfx_test, to verify I can boot and interact with
# the J1B core in a static build before I attempt to live-load this core using
# DFX (Dynamic Function Exchange, a.k.a. Partial FPGA Reconfiguration).
#
# This test also runs on Verilator.

#RAM image variant
add_executable(j1b_test_ram
 EXCLUDE_FROM_ALL
 j1b_test.cpp
)

target_compile_options(j1b_test_ram
 PRIVATE
  -O2 -g -ffunction-sections
)

target_include_directories(j1b_test_ram
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/reset/
)

link_and_create_image(j1b_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(j1b_test_ram sdram gpio riscv j1b_hal bootstrap)

#Flash image variant
add_executable(j1b_test_flsh
 EXCLUDE_FROM_ALL
 j1b_test.cpp
)

target_compile_options(j1b_test_flsh
 PRIVATE
  -O2 -g -ffunction-sections
)

target_include_directories(j1b_test_flsh
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/vs0_hal/
)

link_and_create_image(j1b_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(j1b_test_flsh gpio riscv j1b_hal bootstrap)

add_flash_sw_target(j1b_test_flsh)

