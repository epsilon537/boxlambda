#Real-Time Clock and Calendar Library, interfacing with MCP79412 via I2C

add_library(rtcc)

target_sources(rtcc
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/MCP79412RTC/src/MCP79412RTC.cpp
)

target_compile_options(rtcc
 PRIVATE
  -O2 -g -ffunction-sections
)

target_include_directories(rtcc
    PUBLIC
    ${PROJECT_SOURCE_DIR}/sub/MCP79412RTC/src/
)

target_link_libraries(rtcc i2c timelib)
