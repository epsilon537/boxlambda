# Hardware Access Layer for the J1B DFX demo component.
#
# J1B is a tiny 32-bit stack processor designed to run Forth. J1B is part of
# the SwapForth environment.
#
# For example usage of this HAL, see j1b_cli.cpp and/or j1b_test.cpp.

add_library(j1b_hal)

#j1b_hal hardware access layer.
target_sources(j1b_hal
    PRIVATE
    j1b_hal.c
)

target_compile_options(j1b_hal
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of j1b_hal are expected to use.
target_include_directories(j1b_hal
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
        ${PROJECT_SOURCE_DIR}/sw/components/vs0_hal
)
