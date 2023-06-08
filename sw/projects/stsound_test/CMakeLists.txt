add_executable(stsound_test
	EXCLUDE_FROM_ALL
	stsound_test.cpp
	${PROJECT_SOURCE_DIR}/sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

target_include_directories(stsound_test
    PRIVATE
	${PROJECT_SOURCE_DIR}/sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/
)

target_compile_options(stsound_test
	PRIVATE
		-O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(stsound_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_256K.ld)

target_link_libraries(stsound_test stsound fatfs)
