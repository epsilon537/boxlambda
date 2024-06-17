add_executable(fatfs_test
 EXCLUDE_FROM_ALL
 fatfs_test.c
)

target_compile_options(fatfs_test
 PRIVATE
  -O2 -g
)

target_include_directories(fatfs_test
    PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/platform/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_mem_file(fatfs_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(fatfs_test fatfs mcycle)
