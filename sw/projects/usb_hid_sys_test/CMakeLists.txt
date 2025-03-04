#USB HID system test program - RAM Build

add_executable(usb_hid_sys_test_ram
 EXCLUDE_FROM_ALL
 usb_hid_sys_test.cpp
)

target_compile_options(usb_hid_sys_test_ram
 PRIVATE
  -O2 -g
)

link_and_create_image(usb_hid_sys_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(usb_hid_sys_test_ram usb_hid gpio riscv interrupts bootstrap)

#USB HID system test program - Flash Build

add_executable(usb_hid_sys_test_flsh
 EXCLUDE_FROM_ALL
 usb_hid_sys_test.cpp
)

target_compile_options(usb_hid_sys_test_flsh
 PRIVATE
  -O2 -g
)

link_and_create_image(usb_hid_sys_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(usb_hid_sys_test_flsh usb_hid gpio riscv interrupts bootstrap)

add_flash_sw_target(usb_hid_sys_test_flsh)
