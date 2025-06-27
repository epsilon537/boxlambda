#
# FatFs Test RAM Build
#

add_executable(fatfs_test_ram
 EXCLUDE_FROM_ALL
 fatfs_test.cpp
)

target_compile_options(fatfs_test_ram
 PRIVATE
  -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(fatfs_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(fatfs_test_ram fatfs riscv bootstrap)

#
# FatFs Test Flash Build
#

add_executable(fatfs_test_flsh
 EXCLUDE_FROM_ALL
 fatfs_test.cpp
)

target_compile_options(fatfs_test_flsh
 PRIVATE
  -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(fatfs_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(fatfs_test_flsh fatfs riscv bootstrap)

add_flash_sw_target(fatfs_test_flsh)
