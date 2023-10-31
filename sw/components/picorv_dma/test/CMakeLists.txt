#The following are PicoRV assembly test programs for the programmable picorv_dma core.
#Through the modules in the cmake subdirectory, CMake is aware of the .picoasm extension
#and the ASM_PICO variables selecting the proper toolchain and compile/link flags.

add_executable(picorv_wr_hir_regs
	EXCLUDE_FROM_ALL
    wr_hir_regs.picoasm
)

target_include_directories(picorv_wr_hir_regs
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_wr_hir_regs)

add_executable(picorv_hir_regs_copy
	EXCLUDE_FROM_ALL
    hir_regs_copy.picoasm
)

target_include_directories(picorv_hir_regs_copy
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_hir_regs_copy)

add_executable(picorv_irq_in_out
	EXCLUDE_FROM_ALL
    irq_in_out.picoasm
)

target_include_directories(picorv_irq_in_out
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_irq_in_out)

add_executable(picorv_wordcopy
	EXCLUDE_FROM_ALL
    wordcopy.picoasm
)

target_include_directories(picorv_wordcopy
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_wordcopy)

add_executable(picorv_bytecopy
	EXCLUDE_FROM_ALL
    bytecopy.picoasm
)

target_include_directories(picorv_bytecopy
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_bytecopy)

add_executable(picorv_progmem_data_access
	EXCLUDE_FROM_ALL
    progmem_data_access.picoasm
)

target_include_directories(picorv_progmem_data_access
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_progmem_data_access)

add_executable(picorv_wordcopy_burst
	EXCLUDE_FROM_ALL
    wordcopy_burst.picoasm
)

target_include_directories(picorv_wordcopy_burst
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/..
)

link_gen_picobin(picorv_wordcopy_burst)

