cmake_minimum_required(VERSION 3.24 FATAL_ERROR)
  
project(BoxLambda VERSION 0.1.0 LANGUAGES C ASM)

#Build systems like Ninja can handle multiple configurations in one build tree.
#We currently don't support that.

get_property(isMultiConfig GLOBAL
    PROPERTY GENERATOR_IS_MULTI_CONFIG
)

if(isMultiConfig)
    message(FATAL_ERROR "MultiConfig is not supported.")
endif()

#Currently, two build types are supported: -DCMAKE_BUILD_TYPE=fpga and -DCMAKE_BUILD_TYPE=sim
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY
    STRINGS fpga sim)

set(allowedBuildTypes fpga sim)

if(NOT CMAKE_BUILD_TYPE IN_LIST allowedBuildTypes)
    message(FATAL_ERROR "${CMAKE_BUILD_TYPE} is not a known build type.")
endif()

message("Build Type: ${CMAKE_BUILD_TYPE}.")

enable_testing()

#The software tree
add_subdirectory(sw)

#The gateware tree
add_subdirectory(gw)


