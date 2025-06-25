cmake_minimum_required(VERSION 3.24 FATAL_ERROR)
set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")
project(BoxLambda VERSION 0.1.0 LANGUAGES C CXX ASM)

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

#A codegeneration target. Components that require
#code generation will add their codegeneration target as a
#dependency to this target.
add_custom_target(cgen)

#make regen rule, to completely remove and regenerate the build tree, including
#the code generation (cgen) step.
add_custom_target(regen
    WORKING_DIRECTORY
        ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND
        rm -rf sw && rm -rf gw
    COMMAND
        ${CMAKE_COMMAND} .
    COMMAND
        ${CMAKE_MAKE_PROGRAM} cgen
    COMMENT
        "Removing and regenerating build tree."
    VERBATIM
)

enable_testing()

#The registers tree
add_subdirectory(registers)

#The software tree
add_subdirectory(sw)

#The gateware tree
add_subdirectory(gw)

#A rule to check if the BoxLambda environment is active.
add_custom_target(check_env
    WORKING_DIRECTORY
        ${PROJECT_SOURCE_DIR}/scripts
    COMMAND
        ./check_env.sh
    COMMENT
        "Checking if BoxLambda environment has been activated."
    VERBATIM
)

#Add check_env as a dependency to all targets.
#There has to be a better way to do this but I can't find it. This beats having to add
#the dependency to each target individually.
function (_get_all_cmake_targets out_var current_dir)
    get_property(targets DIRECTORY ${current_dir} PROPERTY BUILDSYSTEM_TARGETS)
    get_property(subdirs DIRECTORY ${current_dir} PROPERTY SUBDIRECTORIES)

    foreach(subdir ${subdirs})
        _get_all_cmake_targets(subdir_targets ${subdir})
        list(APPEND targets ${subdir_targets})
    endforeach()

    set(${out_var} ${targets} PARENT_SCOPE)
endfunction()

# Run at end of top-level CMakeLists
_get_all_cmake_targets(all_targets ${CMAKE_CURRENT_LIST_DIR})

foreach(tgt ${all_targets})
  add_dependencies(${tgt} check_env)
endforeach()

