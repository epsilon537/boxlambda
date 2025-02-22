#
# Timer, UART, GPIO and IRQ test - RAM Build
#
add_executable(timer_uart_gpio_irqs_ram
 EXCLUDE_FROM_ALL
 timer_uart_gpio_irqs.c
)

#Dependency on the PicoRV irq_in_out test program.
add_dependencies(
 timer_uart_gpio_irqs_ram
 picorv_irq_in_out
)

target_compile_options(timer_uart_gpio_irqs_ram
 PRIVATE
  -O2 -g
)

target_include_directories(timer_uart_gpio_irqs_ram
    PRIVATE
    ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_and_create_image(timer_uart_gpio_irqs_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(timer_uart_gpio_irqs_ram timer picorv_dma gpio riscv interrupts bootstrap)

#
# Timer, UART, GPIO and IRQ test - Flash Build
#
add_executable(timer_uart_gpio_irqs_flsh
 EXCLUDE_FROM_ALL
 timer_uart_gpio_irqs.c
)

#Dependency on the PicoRV irq_in_out test program.
add_dependencies(
 timer_uart_gpio_irqs_flsh
 picorv_irq_in_out
)

target_compile_options(timer_uart_gpio_irqs_flsh
 PRIVATE
  -O2 -g
)

target_include_directories(timer_uart_gpio_irqs_flsh
    PRIVATE
    ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_and_create_image(timer_uart_gpio_irqs_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(timer_uart_gpio_irqs_flsh timer picorv_dma gpio riscv interrupts bootstrap)

add_flash_sw_target(timer_uart_gpio_irqs_flsh)
