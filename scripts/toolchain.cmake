# CMake cross-compiler toolchain file.

# the name of the target operating system
set(CMAKE_SYSTEM_NAME Generic)

set(TOOLCHAIN_PREFIX "riscv64-unknown-elf-")

# which compilers to use
set(CMAKE_C_COMPILER riscv64-unknown-elf-gcc)
# enable C++ as well
set(CMAKE_CXX_COMPILER riscv64-unknown-elf-gcc)

#Use GCC for assembly as well.
set(CMAKE_ASM_COMPILER riscv64-unknown-elf-gcc)

# picorv assembler, associated with the .picoasm file extension
# ASM_PICO dialect CMake extension is defined the cmake subdirectory of this repo.
set(CMAKE_ASM_PICO_COMPILER riscv64-unknown-elf-as)

# adjust the default behaviour of the find commands:
# search headers and libraries in the target environment
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
# search programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_ASM_FLAGS "-march=rv32im_zicsr -mabi=ilp32")
set(CMAKE_C_FLAGS "-march=rv32im_zicsr -mabi=ilp32")
# some additional disable options to avoid having to pull in a C++ run-time for exception handling and std c++ library.
set(CMAKE_CXX_FLAGS "-march=rv32im_zicsr -mabi=ilp32 -fno-threadsafe-statics -fno-exceptions")

# picorv assembler flags: base integer ISA, no compression.
# ASM_PICO dialect CMake extension is defined the cmake subdirectory of this repo.
set(CMAKE_ASM_PICO_FLAGS "-march=rv32i")

