# determine the compiler to use for ASM_PICO files

set(ASM_DIALECT "_PICO")
#Use 'as' for assembly and 'ld' for linking. Not GCC.
set(CMAKE_ASM${ASM_DIALECT}_COMPILER_LIST ${_CMAKE_TOOLCHAIN_PREFIX}as)
set(CMAKE_ASM${ASM_DIALECT}_LINKER_LIST ${_CMAKE_TOOLCHAIN_PREFIX}ld)
include(CMakeDetermineASMCompiler)
set(ASM_DIALECT)

