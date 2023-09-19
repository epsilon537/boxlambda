#ifndef PICORV_DMA_HAL_H
#define PICORV_DMA_HAL_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * PicoRV DMA hardware access layer.
 */
#define PICORV_BASE 0x10002000
#define PICORV_PROG_MEM_BASE PICORV_BASE
#define PICORV_SYS_REG_BASE (PICORV_BASE+0x1000)
#define PICORV_GP_REG_BASE (PICORV_SYS_REG_BASE + 16*4)

#define PICORV_SYS_REG_IRQ_OUT 0
#define PICORV_SYS_REG_IRQ_IN 1
#define PICORV_SYS_REG_CTRL 2

inline void picorv_sys_reg_wr(unsigned reg_offset, unsigned val)
{
	((unsigned volatile *)(PICORV_SYS_REG_BASE))[reg_offset] = val;
}

inline unsigned picorv_sys_reg_rd(unsigned reg_offset)
{
	return ((unsigned volatile *)(PICORV_SYS_REG_BASE))[reg_offset];
}

inline void picorv_gp_reg_wr(unsigned gp_reg_offset, unsigned val)
{
	((unsigned volatile *)(PICORV_GP_REG_BASE))[gp_reg_offset] = val;
}

inline unsigned picorv_gp_reg_rd(unsigned gp_reg_offset)
{
	return ((unsigned volatile *)(PICORV_GP_REG_BASE))[gp_reg_offset];
}

//progLen must be a multiple of 4 and <= 0x1000.
void picorv_load_program(unsigned char *progData, unsigned progLen);

#ifdef __cplusplus
}
#endif

#endif //PICORV_DMA_HAL_H