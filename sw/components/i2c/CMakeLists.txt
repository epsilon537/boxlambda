add_library(i2c)

target_sources(i2c
    PRIVATE
    i2c.cpp
)

target_compile_options(i2c
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(i2c
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(i2c riscv)
