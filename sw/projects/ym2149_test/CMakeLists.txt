add_executable(ym2149_test
 EXCLUDE_FROM_ALL
 ym2149_test.c
)

target_compile_options(ym2149_test
 PRIVATE
  -O2
)

target_include_directories(ym2149_test
    PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/ym2149_sys_hal/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_mem_file(ym2149_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(ym2149_test gpio riscv)
