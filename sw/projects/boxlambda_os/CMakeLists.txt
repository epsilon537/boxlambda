#
# BoxLambda Operating System (WIP)
#

# Put the contents of the fs/ directory in an FAT image.

set(FS_IMAGE ${CMAKE_CURRENT_BINARY_DIR}/boxfs.bin)
set(FORTH_DIR ${PROJECT_SOURCE_DIR}/sw/components/forth)

add_custom_command(
      OUTPUT
          ${FS_IMAGE}
      COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/gen_fs_img.sh ${FS_IMAGE} ${FORTH_DIR}/fs
      WORKING_DIRECTORY
          ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT
          "Creating file system image."
      VERBATIM
  )

add_custom_target(boxfs
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS
        ${FS_IMAGE}
    COMMENT
        "BoxLambda FS image generation target."
)

add_executable(boxkern
   main.cpp
)

target_compile_options(boxkern
 PRIVATE
  -O2 -g
)

target_include_directories(boxkern
  PRIVATE
   ${FORTH_DIR}/core
   ${FORTH_DIR}/fs/forth
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(boxkern ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)

target_link_libraries(boxkern fatfs forth_core gpio bootstrap)

add_flash_sw_target(boxkern)

