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

function(add_boxlambda_kernel TARGET)
    set(oneValueArgs)
    set(multiValueArgs SOURCES)
    cmake_parse_arguments(KERNEL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    add_executable(${TARGET} ${KERNEL_SOURCES})

    target_compile_options(${TARGET}
        PRIVATE
            -O2 -g
    )

    target_include_directories(${TARGET}
        PRIVATE
            ${PROJECT_SOURCE_DIR}/sw/components/common
    )

    target_link_libraries(${TARGET}
        fatfs
        forth_core
        gpio
        bootstrap
    )

    link_and_create_image(${TARGET} ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)
endfunction()

add_boxlambda_kernel(boxkern
    SOURCES
        main.cpp
)

add_flash_sw_target(boxkern)

add_boxlambda_kernel(boxkerntest
    SOURCES
        main.cpp
        forth_core_test.cpp
)

target_compile_definitions(boxkerntest PRIVATE FORTH_CORE_TEST)

function(add_kernel_fs_targets TARGET)
    add_custom_target(${TARGET}fs
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${TARGET} ${FS_IMAGE}
        COMMENT "BoxLambda ${TARGET} and FS image generation target."
    )

    add_custom_target(${TARGET}fs_load
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py
                -reset
                -load_fs ${FS_IMAGE}
                -load_app ${TARGET}.bin
                -run
        DEPENDS ${TARGET}fs
        COMMENT "BoxLambda ${TARGET} and FS image loading target."
    )
endfunction()

add_kernel_fs_targets(boxkern)
add_kernel_fs_targets(boxkerntest)

