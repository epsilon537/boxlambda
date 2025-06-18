add_library(sdspi)

target_sources(sdspi
    PRIVATE
    sdcard.c
)

target_compile_definitions(sdspi
 PRIVATE
  -DSDSPI_CPU_IS_LITTLE_ENDIAN
)

target_compile_options(sdspi
 PRIVATE
  -O2 -g -ffunction-sections
)

target_include_directories(sdspi
    PUBLIC
        ${PROJECT_SOURCE_DIR}/registers/generated/
        ${CMAKE_CURRENT_LIST_DIR}
)

