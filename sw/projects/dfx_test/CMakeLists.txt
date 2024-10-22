# DFX test program to run on the dfx_test gateware build. The program presents
# a CLI through which the user can live-load reconfigurable modules such as the
# vs0_j1b core into the VS0 'black box' slot/partition of the BoxLambda SoC.

# Note that this directory also contains a nuc.bin file. This is the Forth run-time firmware image to be loaded into
# the J1B core after it has been DFX loaded into the SoC.

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
  ${PROJECT_SOURCE_DIR}/sw/components/dfx/
  ${PROJECT_SOURCE_DIR}/sw/components/vs0_hal/
)

link_and_create_mem_file(dfx_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(dfx_test sdram gpio embedded_cli riscv interrupts peek_poke_cli j1b_hal fatfs ymodem_cli mem_fs_cli)

add_flash_sw_target(dfx_test)

