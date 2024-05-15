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

link_internal_create_mem_file(usb_hid_sys_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(usb_hid_sys_test usb_hid)
