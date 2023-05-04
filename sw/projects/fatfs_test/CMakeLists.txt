add_executable(fatfs_test
	EXCLUDE_FROM_ALL
	fatfs_test.c
)

target_compile_definitions(fatfs_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
)

target_compile_options(fatfs_test
	PRIVATE
		-O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(fatfs_test)

target_link_libraries(fatfs_test fatfs)
