cmake_minimum_required(VERSION 3.24 FATAL_ERROR)
  
project(BoxLambda VERSION 0.1.0 LANGUAGES C ASM)

get_property(isMultiConfig GLOBAL
    PROPERTY GENERATOR_IS_MULTI_CONFIG
)

if(isMultiConfig)
    message(FATAL_ERROR "MultiConfig is not supported.")
endif()

set_property(CACHE CMAKE_BUILD_TYPE PROPERTY
    STRINGS fpga sim)

set(allowedBuildTypes fpga sim)

if(NOT CMAKE_BUILD_TYPE IN_LIST allowedBuildTypes)
    message(FATAL_ERROR "${CMAKE_BUILD_TYPE} is not a known build type.")
endif()

message("Build Type: ${CMAKE_BUILD_TYPE}.")

add_subdirectory(sw)


