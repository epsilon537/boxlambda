#
# BoxLambda Operating System (WIP)
#

# Put the contents of the fs/ directory in an FAT image.
set(FORTH_FS_DIR ${PROJECT_SOURCE_DIR}/fs)

add_custom_target(
  boxfs_load
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py -load_fs ${FORTH_FS_DIR}
  COMMENT "BoxLambda FS image loading target.")

add_executable(
  boxkern
  main.cpp
  forth_core_test.cpp)

target_compile_options(
  boxkern
  PRIVATE -O2
          -g)

target_include_directories(boxkern
                           PRIVATE ${PROJECT_SOURCE_DIR}/sw/components/common)

target_link_libraries(
  boxkern
  fatfs
  forth
  gpio
  bootstrap)

link_and_create_image(
  boxkern
  ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)

# FASTBOOT_OPT=2 corresponds to FASTBOOT_OPT_COMPILE
target_compile_definitions(boxkern PRIVATE FASTBOOT_OPT=2)

file(
  GLOB_RECURSE
  BOXKERN_FORTH_SOURCES
  CONFIGURE_DEPENDS
  "${PROJECT_SOURCE_DIR}/fs/forth/boxkern-includes/*.fs")

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth.img
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py -reset -load_fs
          ${FORTH_FS_DIR} -load_app boxkern.bin -run
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py -wait_for_status 1
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py -dump_fs fs
  COMMAND mv ${CMAKE_CURRENT_BINARY_DIR}/fs/boxkern-forth.img boxkern-forth.img
  DEPENDS boxkern
          ${BOXKERN_FORTH_SOURCES}
  COMMENT "Compiling boxkern-forth.img")

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth-img.o
  COMMAND
    ${CMAKE_OBJCOPY} -I binary -O elf32-littleriscv -B riscv --rename-section
    .data=.boxkern_forth_image,alloc,load,readonly,data,contents
    ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth.img
    ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth-img.o
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth.img)

add_custom_target(boxkernforth_compile
                  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth-img.o)

add_executable(
  boxkernforth
  main.cpp
  forth_core_test.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/boxkern-forth-img.o)

target_compile_options(
  boxkernforth
  PRIVATE -O2
          -g)

target_include_directories(boxkern
                           PRIVATE ${PROJECT_SOURCE_DIR}/sw/components/common)

target_link_libraries(
  boxkernforth
  fatfs
  forth
  gpio
  bootstrap)

link_and_create_image(
  boxkernforth
  ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)

# FASTBOOT_OPT=1 corresponds to FASTBOOT_OPT_LOAD
target_compile_definitions(boxkernforth PRIVATE FASTBOOT_OPT=1)

add_custom_target(
  boxkernforth_fs_load
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py -reset -load_fs
          ${FORTH_FS_DIR} -load_app boxkernforth.bin -run
  DEPENDS boxkernforth
  COMMENT "Loading boxkern-forth with ramdisk.")
