#gw_component_rules() function is defined in the parent CMakeLists.txt.
gw_component_rules(
    TOP_MODULE  picorv_dma_top
    COMPONENT_NAME picorv_dma 
)

add_custom_target(picorv_dma_test
        WORKING_DIRECTORY
            ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT
            "Dummy target to collect test dependencies."
        DEPENDS
            picorv_wr_gp_regs
            picorv_gp_regs_copy
            picorv_irq_in_out
            picorv_wordcopy
            picorv_bytecopy
        VERBATIM
    )

add_test(NAME picorv_dma_cocotb_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/picorv_dma_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

