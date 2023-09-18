add_executable(picorv_wr_gp_regs
	EXCLUDE_FROM_ALL
    wr_gp_regs.picoasm
)

link_gen_picobin(picorv_wr_gp_regs)

add_executable(picorv_gp_regs_copy
	EXCLUDE_FROM_ALL
    gp_regs_copy.picoasm
)

link_gen_picobin(picorv_gp_regs_copy)

add_executable(picorv_irq_in_out
	EXCLUDE_FROM_ALL
    irq_in_out.picoasm
)

link_gen_picobin(picorv_irq_in_out)