# This file is used by EnableLanguage in cmGlobalGenerator to
# determine that the selected ASM_PICO "compiler" works.
# For assembler this can only check whether the compiler has been found,
# because otherwise there would have to be a separate assembler source file
# for each assembler on every architecture.

set(ASM_DIALECT "_PICO")
include(CMakeTestASMCompiler)
set(ASM_DIALECT)
