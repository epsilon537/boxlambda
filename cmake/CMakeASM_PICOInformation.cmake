# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.


# support for AT&T syntax assemblers, e.g. GNU as

set(ASM_DIALECT "_PICO")
# *.S files are supposed to be preprocessed, so they should not be passed to
# assembler but should be processed by gcc
set(CMAKE_ASM${ASM_DIALECT}_SOURCE_FILE_EXTENSIONS picoasm)

set(CMAKE_ASM${ASM_DIALECT}_COMPILE_OBJECT "<CMAKE_ASM${ASM_DIALECT}_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
set(CMAKE_ASM${ASM_DIALECT}_LINK_EXECUTABLE "<CMAKE_LINKER> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")

include(CMakeASMInformation)
set(ASM_DIALECT)
