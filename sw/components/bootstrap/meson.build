bootstrap_inc_dirs = include_directories(
    '../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc'
    )

bootstrap_srcs = files(
    'vectors.S', 
    'stdio_to_uart.c', 
    '../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/uart.c'
    )

bootstrap_lib = static_library(
    'bootstrap', 
    bootstrap_srcs, 
    include_directories: bootstrap_inc_dirs
    )