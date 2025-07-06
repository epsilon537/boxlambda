#Second stage bootloader runs from DDR memory
add_executable(second_stage_boot
    EXCLUDE_FROM_ALL
      second_stage_boot.cpp
)

target_compile_options(second_stage_boot
 PRIVATE -g)

target_link_libraries(second_stage_boot gpio uart bootstrap riscv)

target_include_directories(second_stage_boot
  PUBLIC
    ${PROJECT_SOURCE_DIR}/sw/components/memmap
    ${PROJECT_SOURCE_DIR}/sw/components/flashdrvr
    ${PROJECT_SOURCE_DIR}/sw/components/memmap/
)

link_and_create_image(second_stage_boot
${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_boot.ld)

#Command to convert second stage boot .bin to a .o object file containing the
#image data.
add_custom_command(
  OUTPUT second_stage_boot_img.o
  COMMAND
    ${CMAKE_LINKER} -r -b binary -o second_stage_boot_img.o second_stage_boot.bin
  WORKING_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
    second_stage_boot
)

#First stage bootloader boots from IMEM
add_executable(bootloader
    EXCLUDE_FROM_ALL
      first_stage_boot.cpp
      second_stage_boot_img.o
)

target_compile_options(bootloader
 PRIVATE -g)

target_link_libraries(bootloader
  gpio uart bootstrap sdram riscv)

target_include_directories(bootloader
  PUBLIC
    ${PROJECT_SOURCE_DIR}/sw/components/imem
    ${PROJECT_SOURCE_DIR}/sw/components/flashdrvr
    ${PROJECT_SOURCE_DIR}/sw/components/memmap/
)

link_and_create_image(bootloader ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

#Copy bootloader.mem to imem.mem. It's the default
#imem.mem image on FPGA builds.
add_custom_command(
  TARGET bootloader
  POST_BUILD
  WORKING_DIRECTORY
      ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND cp -f bootloader.mem imem.mem
  COMMENT "Copying bootloader.mem to imem.mem"
  VERBATIM
)

#Add bootloader to codegeneration dependency list.
#It's the default IMEM .mem file.
add_dependencies(cgen bootloader)

#The bootloader requires the litedram core to be generated first.
add_dependencies(bootloader gen_litedram_core)

