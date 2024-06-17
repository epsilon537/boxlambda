
add_executable(gpios_timers_and_irqs
 EXCLUDE_FROM_ALL
 gpios_timers_and_irqs.c
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

#Dependency on the PicoRV irq_in_out test program.
add_dependencies(
 gpios_timers_and_irqs
 picorv_irq_in_out
)

target_compile_options(gpios_timers_and_irqs
 PRIVATE
  -O2 -g
)

target_include_directories(gpios_timers_and_irqs
    PRIVATE
    ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_and_create_mem_file(gpios_timers_and_irqs
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(gpios_timers_and_irqs timer picorv_dma gpio)

