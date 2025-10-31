#
# Time RTC Set Test
#
add_executable(time_rtc_set
 EXCLUDE_FROM_ALL
 time_rtc_set.cpp
)

target_compile_options(time_rtc_set
 PRIVATE
  -O2 -g
)

link_and_create_image(time_rtc_set
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)

target_link_libraries(time_rtc_set rtcc i2c gpio riscv timelib bootstrap)

target_include_directories(time_rtc_set
    PUBLIC
       ${PROJECT_SOURCE_DIR}/sw/components/arduino
       ${PROJECT_SOURCE_DIR}/sw/components/timer
)

