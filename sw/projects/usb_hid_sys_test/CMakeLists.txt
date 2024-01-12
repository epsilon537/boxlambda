#USB HID system test program

add_executable(usb_hid_sys_test
	EXCLUDE_FROM_ALL
	usb_hid_sys_test.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

target_compile_options(usb_hid_sys_test
	PRIVATE
		-O2 -g
)

target_include_directories(usb_hid_sys_test
    PRIVATE
	${PROJECT_SOURCE_DIR}/sw/components/usb_hid
)

link_internal_create_mem_file(usb_hid_sys_test 
	${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)
