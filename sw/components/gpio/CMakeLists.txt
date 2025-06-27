add_library(gpio)

target_sources(gpio
    PRIVATE
    gpio.cpp
)

target_compile_options(gpio
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(gpio
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
        ${PROJECT_SOURCE_DIR}/registers/generated/
)
