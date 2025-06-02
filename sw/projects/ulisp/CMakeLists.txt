#
# ulisp - RAM Build
#
add_executable(ulisp_ram
 EXCLUDE_FROM_ALL
 ulisp-riscv-comments.cpp
 main.cpp
)

target_compile_options(ulisp_ram
 PRIVATE
  -O2
)

target_compile_definitions(ulisp_ram
 PRIVATE
  -DBOARD_BOXLAMBDA
)

# target_include_directories(ulisp_ram
#   PRIVATE
#   ${PROJECT_SOURCE_DIR}/sw/components/i2c/
# )

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ulisp_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(ulisp_ram arduino gpio riscv vera bootstrap timer
interrupts sdram)

#
# ulisp - Flash Build
#
add_executable(ulisp_flsh
 EXCLUDE_FROM_ALL
 ulisp-riscv-comments.cpp
 main.cpp
)

target_compile_options(ulisp_flsh
 PRIVATE
  -O2
)

target_compile_definitions(ulisp_flsh
 PRIVATE
  -DBOARD_BOXLAMBDA
)

# target_include_directories(ibex_perf_test_flsh
#   PRIVATE
#   ${PROJECT_SOURCE_DIR}/sw/components/i2c/
# )

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ulisp_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(ulisp_flsh arduino gpio riscv vera bootstrap timer
interrupts sdram)

add_flash_sw_target(ulisp_flsh)

