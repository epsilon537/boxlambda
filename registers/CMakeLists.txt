set(register_modules
    uart
    reset
    gpio
    mtimer
    i2c_master
    usb_hid
    sdspi
    spiflash
    vera
    vera_spr_ram
    ym2149
    dfx_controller_other
    dfx_controller_ctrl)

set(memmap_yaml ${CMAKE_CURRENT_SOURCE_DIR}/definitions/memmap.yaml)
set(c_header ${CMAKE_CURRENT_SOURCE_DIR}/generated/memmap.h)
set(forth_header ${PROJECT_SOURCE_DIR}/fs/forth/boxkern-includes/memmap.fs)

# A codegeneration rule to generate the register access layer
add_custom_target(
  registers
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/gen_registers.sh ${register_modules}
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/gen_memmap.py -i ${memmap_yaml} -c
          ${c_header} -f ${forth_header}
  COMMENT "Generating Register Access Layer."
  VERBATIM)

# Add registers target to the codegeneration dependency list.
add_dependencies(
  cgen
  registers)
