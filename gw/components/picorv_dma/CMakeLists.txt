#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE  picorv_dma_top
    COMPONENT_NAME picorv_dma 
)

#This is a dummy target. It doesn't build anything. It just collects the picorv test programs
#as dependencies for the testcase below. Sadly, add_test() doesn't have a DEPENDS attribute.
#This means you need to run 'make picorv_dma_test', before running ctest.
add_custom_target(picorv_dma_test ALL
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT
            "Dummy target to collect test dependencies."
        DEPENDS
            picorv_wr_hir_regs
            picorv_hir_regs_copy
            picorv_irq_in_out
            picorv_wordcopy_single
            picorv_bytecopy_single
            picorv_progmem_data_access
            picorv_burst_fsm_test
        VERBATIM
    )

#A CocoTB-based test program.
add_test(NAME picorv_dma_cocotb_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/picorv_dma_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

#A CocoTB-based test program.
add_test(NAME picorv_burst_fsm_cocotb_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/picorv_burst_fsm_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

