#Peek/Poke CLI commands

add_library(peek_poke_cli)

target_sources(peek_poke_cli
    PRIVATE
    peek_poke_cli.cpp
)

target_compile_options(peek_poke_cli
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(peek_poke_cli
    PUBLIC
       ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(peek_poke_cli embedded_cli riscv)
