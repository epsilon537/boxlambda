add_executable(vera_test
 EXCLUDE_FROM_ALL
 vera_test.c
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

target_compile_options(vera_test
 PRIVATE
  -O2
)

#Function defined in parent CMakeLists.txt file:
link_and_create_mem_file(vera_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(vera_test vera sdram gpio)
