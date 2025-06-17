set(register_modules uart reset gpio mtimer i2c_master usb_hid)

#A codegeneration rule to generate the register access layer
add_custom_target(registers
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/gen_registers.sh ${register_modules}
    COMMENT
      "Generating Register Access Layer."
    VERBATIM
)

#Add registers target to the codegeneration dependency list.
add_dependencies(cgen registers)
