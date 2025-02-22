#
# VERA Test- RAM Build
#
add_executable(vera_test_ram
 EXCLUDE_FROM_ALL
 vera_test.c
)

target_compile_options(vera_test_ram
 PRIVATE
  -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(vera_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(vera_test_ram vera sdram gpio riscv interrupts bootstrap)

#
# VERA Test- Flash Build
#
add_executable(vera_test_flsh
 EXCLUDE_FROM_ALL
 vera_test.c
)

target_compile_options(vera_test_flsh
 PRIVATE
  -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(vera_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(vera_test_flsh vera sdram gpio riscv interrupts bootstrap)

add_flash_sw_target(vera_test_flsh)
