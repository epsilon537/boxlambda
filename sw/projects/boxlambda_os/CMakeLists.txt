#
# BoxLambda Operating System (WIP)
#

# Put the contents of the fs/ directory in an FAT image.

set(FS_IMAGE ${CMAKE_CURRENT_BINARY_DIR}/boxfs.bin)
set(FORTH_DIR ${PROJECT_SOURCE_DIR}/sw/components/forth)

# Dummy output which is never actually produced. Anything that depends on
# this will always be rebuilt.
add_custom_command(
    OUTPUT always_rebuild
    COMMAND cmake -E echo
    COMMENT
        "always_rebuild dummy command."
)

add_custom_command(
      OUTPUT
          ${FS_IMAGE}
      COMMAND
          ${PROJECT_SOURCE_DIR}/scripts/gen_fs_img.sh ${FS_IMAGE} ${FORTH_DIR}/fs
      DEPENDS
          always_rebuild
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

add_custom_target(boxfs_load
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/target.py -load_fs ${FS_IMAGE}
    DEPENDS
        boxfs
    COMMENT
        "BoxLambda FS image loading target."
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

add_custom_target(boxkernfs
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS
        boxkern ${FS_IMAGE}
    COMMENT
        "BoxLambda Kernel and FS image generation target."
)

add_custom_target(boxkernfs_load
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/target.py -load_fs ${FS_IMAGE} -load_app boxkern.bin -run
    DEPENDS
        boxkernfs
    COMMENT
        "BoxLambda Kernel and FS image loading target."
)

