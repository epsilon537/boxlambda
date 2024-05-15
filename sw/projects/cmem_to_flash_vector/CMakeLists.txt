# This SW project builds the startup vector code intended for initial CMEM RAM contents.
# The Ibex RISCV CPU boots from 0x80, which is located in CMEM.
# The vector code initializes CPU registers and branches to flash-based startup code (0x11400000).
# The same vector.S code is used as for SW builds that are linked to boot from CMEM.

add_executable(cmem_to_flash_vector
    EXCLUDE_FROM_ALL
      ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/vectors.S)

#Externally define some linker symbols: _start, __cmem_size and __dmem_size.
target_link_options(cmem_to_flash_vector
    PRIVATE
      "LINKER:--defsym=_start=0x11400000"
      "LINKER:--defsym=__cmem_size=${CMEM_SIZE}"
      "LINKER:--defsym=__dmem_size=${DMEM_SIZE}"
      "LINKER:--gc-sections"
      -T${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld
      "LINKER:--Map,${CMAKE_CURRENT_BINARY_DIR}/cmem_to_flash_vector.map"
)

#.mem and .bin file generation commands.
add_custom_command(
    TARGET
      cmem_to_flash_vector
    POST_BUILD
    COMMAND
      ${CMAKE_OBJCOPY} -O verilog --interleave-width=4 --interleave=4 --byte=0 cmem_to_flash_vector cmem_to_flash_vector.hex
    COMMAND
      ${CMAKE_OBJCOPY} -O binary cmem_to_flash_vector cmem_to_flash_vector.bin
    COMMAND
      ${PROJECT_SOURCE_DIR}/sub/ibex_wb/scripts/hex2vmem.pl < cmem_to_flash_vector.hex > cmem_to_flash_vector.mem
    WORKING_DIRECTORY
      ${CMAKE_CURRENT_BINARY_DIR}
    BYPRODUCTS
      cmem_to_flash_vector.hex cmem_to_flash_vector.mem
    COMMENT
      "Converting executable to mem and bin."
    VERBATIM
)

