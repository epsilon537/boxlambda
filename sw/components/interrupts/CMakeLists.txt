add_library(interrupts)

target_sources(interrupts
    PRIVATE
    interrupts.c
)

target_compile_options(interrupts
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(interrupts
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
        ${PROJECT_SOURCE_DIR}/sw/components/riscv
)
