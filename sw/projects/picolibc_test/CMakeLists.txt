add_executable(picolibc_test
	EXCLUDE_FROM_ALL
	picolibc_test.c
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(picolibc_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)
