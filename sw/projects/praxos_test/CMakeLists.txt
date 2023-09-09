add_custom_command(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/praxos_wordcopy.h
  COMMAND
  	${PROJECT_SOURCE_DIR}/scripts/praxos_asm.sh ${PROJECT_SOURCE_DIR}/gw/components/praxos/test/praxos_wordcopy.pxasm
  WORKING_DIRECTORY
  	${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  	${PROJECT_SOURCE_DIR}/scripts/praxos_asm.sh
	${PROJECT_SOURCE_DIR}/sub/Praxos/sw/src/praxos_asm.py
  )

add_executable(praxos_test
	EXCLUDE_FROM_ALL
	main.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
	${CMAKE_CURRENT_BINARY_DIR}/praxos_wordcopy.h
)

target_compile_definitions(praxos_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
)

target_compile_options(praxos_test
	PRIVATE
		-O2
)

#Not sure if this is needed.
target_include_directories(praxos_test
	PRIVATE
    	${CMAKE_CURRENT_BINARY_DIR}
)

target_include_directories(praxos_test
    PRIVATE
	${PROJECT_SOURCE_DIR}/sw/components/praxos_hal/
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(praxos_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)
