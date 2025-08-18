add_library(arduino)

target_sources(arduino
    PRIVATE
    Serial.cpp
    Wire.cpp
    GPIO.cpp
    SPI.cpp
)

target_compile_options(arduino
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(arduino
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(arduino uart bootstrap riscv timer)
