add_custom_target(gen_version_h
    COMMAND
      ${PROJECT_SOURCE_DIR}/scripts/gen_version_h.py > ${CMAKE_CURRENT_BINARY_DIR}/version.h
      "Generating version.h"
    VERBATIM
)

#First stage bootloader boots from IMEM
add_executable(bootloader
    EXCLUDE_FROM_ALL
      bootloader.cpp
)

target_compile_definitions(bootloader
  PUBLIC
    -DPROJECT_VERSION=${PROJECT_VERSION}
)

target_compile_options(bootloader
 PRIVATE -g)

target_link_libraries(bootloader
  gpio uart bootstrap sdram riscv)

target_include_directories(bootloader
  PUBLIC
    ${PROJECT_SOURCE_DIR}/sw/components/bootstrap
    ${PROJECT_SOURCE_DIR}/sw/components/imem
    ${PROJECT_SOURCE_DIR}/sw/components/flashdrvr
    ${PROJECT_SOURCE_DIR}/sw/components/memmap/
    ${CMAKE_CURRENT_BINARY_DIR}
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(bootloader
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(bootloader ${CMAKE_CURRENT_SOURCE_DIR}/link_bootloader.ld)
endif()

if (BL_TARGET_FPGA STREQUAL "arty-a7-100")
  set(_board arty_a7_100t)
else()
  set(_board arty_a7_35t)
endif()

add_custom_target(bootloader_flash_sw
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/flash_sw.sh -b ${_board} bootloader.bin
      COMMENT
          "Flashing bootloader to target."
      VERBATIM
)

#The bootloader requires the litedram core and the version.h file to be generated first.
add_dependencies(bootloader gen_litedram_core gen_version_h)

