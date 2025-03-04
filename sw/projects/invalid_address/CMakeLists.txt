#
# WB Error Handling Test RAM Build
#

add_executable(invalid_address_ram
 EXCLUDE_FROM_ALL
    invalid_address.c
)

target_compile_options(invalid_address_ram
 PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(invalid_address_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(invalid_address_ram gpio riscv bootstrap)

#
# WB Error Handling Test Flash Build
#

add_executable(invalid_address_flsh
 EXCLUDE_FROM_ALL
    invalid_address.c
)

target_compile_options(invalid_address_flsh
 PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(invalid_address_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(invalid_address_flsh gpio riscv bootstrap)

add_flash_sw_target(invalid_address_flsh)

