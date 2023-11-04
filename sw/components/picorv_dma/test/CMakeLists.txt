#The following are PicoRV assembly test programs for the programmable picorv_dma core.
#Through the modules in the cmake subdirectory, CMake is aware of the .picoasm extension
#and the ASM_PICO variables selecting the proper toolchain and compile/link flags.

build_picoasm(picorv_wr_hir_regs wr_hir_regs.picoasm)

target_include_directories(picorv_wr_hir_regs
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_hir_regs_copy hir_regs_copy.picoasm)

target_include_directories(picorv_hir_regs_copy
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_irq_in_out irq_in_out.picoasm)

target_include_directories(picorv_irq_in_out
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_wordcopy_single wordcopy_single.picoasm)

target_include_directories(picorv_wordcopy_single
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_wordcopy_single_unrolled wordcopy_single_unrolled.picoasm)

target_include_directories(picorv_wordcopy_single_unrolled
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_bytecopy_single bytecopy_single.picoasm)

target_include_directories(picorv_bytecopy_single
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_progmem_data_access progmem_data_access.picoasm)

target_include_directories(picorv_progmem_data_access
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_burst_fsm_test burst_fsm_test.picoasm)

target_include_directories(picorv_burst_fsm_test
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_wordcopy_burst wordcopy_burst.picoasm)

target_include_directories(picorv_wordcopy_burst
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

build_picoasm(picorv_bytecopy_burst bytecopy_burst.picoasm)

target_include_directories(picorv_bytecopy_burst
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)
