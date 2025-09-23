#USB HID system test program

add_executable(usb_hid_sys_test
 EXCLUDE_FROM_ALL
 usb_hid_sys_test.cpp
)

target_compile_options(usb_hid_sys_test
 PRIVATE
  -O2 -g
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(usb_hid_sys_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(usb_hid_sys_test
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)
endif()

target_link_libraries(usb_hid_sys_test usb_hid gpio riscv interrupts bootstrap)

