cmake_minimum_required(VERSION 3.24 FATAL_ERROR)

project(BoxLambda VERSION 0.1.0 LANGUAGES C CXX ASM)

#Add the cmake subdirectory to CMake's module path. The ASM_PICO dialect is defined there.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake)

#Enable picorv32 assembler dialect, associated with .picoasm file extension.
#We want to keep this separate from the regular host processor assembler settings associated
#with .s and .S files.
enable_language(ASM_PICO)

#Build systems like Ninja can handle multiple configurations in one build tree.
#We currently don't support that.

get_property(isMultiConfig GLOBAL
    PROPERTY GENERATOR_IS_MULTI_CONFIG
)

if(isMultiConfig)
    message(FATAL_ERROR "MultiConfig is not supported.")
endif()

#Currently, three build types are supported: openxc7, vivado, and verilator
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY
    STRINGS openxc7 vivado sim)

set(allowedBuildTypes openxc7 vivado verilator)

if(NOT CMAKE_BUILD_TYPE IN_LIST allowedBuildTypes)
    message(FATAL_ERROR "${CMAKE_BUILD_TYPE} is not a known build type.")
endif()

message("Build Type: ${CMAKE_BUILD_TYPE}.")

#make regen rule, to complete remove and regenerate the build tree.
add_custom_target(regen
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND
        rm -rf sw && rm -rf gw
    COMMAND
        ${CMAKE_COMMAND} .
    COMMENT
        "Removing and regenerating build tree."
    VERBATIM
)

enable_testing()

#The software tree
add_subdirectory(sw)

#The gateware tree
add_subdirectory(gw)


