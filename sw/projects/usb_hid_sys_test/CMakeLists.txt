#USB HID system test program

add_executable(usb_hid_sys_test
 EXCLUDE_FROM_ALL
 usb_hid_sys_test.cpp
)

target_compile_options(usb_hid_sys_test
 PRIVATE
  -O2 -g
)

link_and_create_mem_file(usb_hid_sys_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(usb_hid_sys_test usb_hid gpio riscv interrupts)
