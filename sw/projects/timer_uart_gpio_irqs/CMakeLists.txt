
add_executable(timer_uart_gpio_irqs
 EXCLUDE_FROM_ALL
 timer_uart_gpio_irqs.c
)

#Dependency on the PicoRV irq_in_out test program.
add_dependencies(
 timer_uart_gpio_irqs
 picorv_irq_in_out
)

target_compile_options(timer_uart_gpio_irqs
 PRIVATE
  -O2 -g
)

target_include_directories(timer_uart_gpio_irqs
    PRIVATE
    ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_and_create_mem_file(timer_uart_gpio_irqs
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(timer_uart_gpio_irqs timer picorv_dma gpio riscv interrupts)

