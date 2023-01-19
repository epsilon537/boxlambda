add_library(bootstrap OBJECT)

target_sources(bootstrap
    PRIVATE
        vectors.S
        stdio_to_uart.c
        ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/uart.c
)

target_include_directories(bootstrap
    PUBLIC
        ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc
        ${CMAKE_CURRENT_LIST_DIR}
)
