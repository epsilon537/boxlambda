#
# BoxLambda Operating System (WIP)
#

# Put the contents of the fs/ directory in an FAT image.
set(FORTH_FS_DIR ${PROJECT_SOURCE_DIR}/fs)

add_custom_target(boxfs_load
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND
        ${PROJECT_SOURCE_DIR}/scripts/target.py -load_fs ${FORTH_FS_DIR}
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
        forth
        gpio
        bootstrap
    )

    link_and_create_image(${TARGET} ${CMAKE_CURRENT_SOURCE_DIR}/link.ld)
endfunction()

# Two BoxLambda kernel variants:

# 1. boxkern: the regular BoxLambda kernel
add_boxlambda_kernel(boxkern
    SOURCES
        main.cpp
)

add_flash_sw_target(boxkern)

# 2. boxkerntest: the BoxLambda kernel + testsuite
add_boxlambda_kernel(boxkerntest
    SOURCES
        main.cpp
        forth_core_test.cpp
)

# boxkerntest builds all the same files as boxkern, plus one
# additional file, forth_core_test.cpp, and with the following flag set:
target_compile_definitions(boxkerntest PRIVATE FORTH_CORE_TEST)

function(add_kernel_fs_targets TARGET)
    add_custom_target(${TARGET}fs_load
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${PROJECT_SOURCE_DIR}/scripts/target.py
                -reset
                -load_fs ${FORTH_FS_DIR}
                -load_app ${TARGET}.bin
                -run
        DEPENDS ${TARGET}
        COMMENT "BoxLambda ${TARGET} and FS image loading target."
    )
endfunction()

# Add ramdisk filesystem image building and loading target *fs_load for
# the regular boxkern and for the boxkern-with-testsuite.
add_kernel_fs_targets(boxkern)
add_kernel_fs_targets(boxkerntest)

