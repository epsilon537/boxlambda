# CMake cross-compiler toolchain file.

# the name of the target operating system
set(CMAKE_SYSTEM_NAME Generic)

# which compilers to use
set(CMAKE_C_COMPILER riscv32-unknown-elf-gcc)
# enable C++ as well
set(CMAKE_CXX_COMPILER riscv32-unknown-elf-gcc)

# adjust the default behaviour of the find commands:
# search headers and libraries in the target environment
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
# search programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_C_FLAGS_INIT "-msave-restore -fshort-enums -march=rv32imc -mabi=ilp32")
# some additional disable options to avoid having to pull in a C++ run-time for exception handling and std c++ library.
set(CMAKE_CXX_FLAGS_INIT "-msave-restore -fshort-enums -march=rv32imc -mabi=ilp32 -fno-threadsafe-statics -fno-exceptions")
