# CMake cross-compiler toolchain file.

# the name of the target operating system
set(CMAKE_SYSTEM_NAME Generic)

# which compilers to use
set(CMAKE_C_COMPILER riscv32-unknown-elf-gcc)

# adjust the default behaviour of the find commands:
# search headers and libraries in the target environment
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
# search programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_C_FLAGS_INIT "-msave-restore -fshort-enums -march=rv32imc -mabi=ilp32")
