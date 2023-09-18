# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.


# determine the compiler to use for ASM using AT&T syntax, e.g. GNU as

set(ASM_DIALECT "_PICO")
set(CMAKE_ASM${ASM_DIALECT}_COMPILER_LIST ${_CMAKE_TOOLCHAIN_PREFIX}as)
set(CMAKE_ASM${ASM_DIALECT}_LINKER_LIST ${_CMAKE_TOOLCHAIN_PREFIX}ld)
include(CMakeDetermineASMCompiler)
set(ASM_DIALECT)

