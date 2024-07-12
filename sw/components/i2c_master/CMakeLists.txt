add_library(i2c_master)

target_sources(i2c_master
    PRIVATE
    i2c_master_hal.c
)

target_compile_options(i2c_master
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(i2c_master
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
