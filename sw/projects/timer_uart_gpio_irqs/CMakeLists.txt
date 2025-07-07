#
# Timer, UART, GPIO and IRQ test - RAM Build
#
add_executable(timer_uart_gpio_irqs
 EXCLUDE_FROM_ALL
 timer_uart_gpio_irqs.cpp
)

target_compile_options(timer_uart_gpio_irqs
 PRIVATE
  -O2 -g
)

link_and_create_image(timer_uart_gpio_irqs
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(timer_uart_gpio_irqs timer gpio riscv interrupts bootstrap)

