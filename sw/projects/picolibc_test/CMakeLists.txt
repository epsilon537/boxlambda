add_executable(picolibc_test
	EXCLUDE_FROM_ALL
	picolibc_test.c
)

link_internal_create_mem(picolibc_test)
