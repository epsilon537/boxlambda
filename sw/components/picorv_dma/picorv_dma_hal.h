#ifndef PICORV_DMA_HAL_H
#define PICORV_DMA_HAL_H

#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * PicoRV DMA hardware access layer.
 */
#define PICORV_BASE 0x10002000

//Program Memory Base Address and size
#define PICORV_PROG_MEM_BASE PICORV_BASE
#define PICORV_PROG_SIZE_BYTES 4096

//System Registers Base Address.
#define PICORV_SYS_REG_BASE (PICORV_BASE+0x1000)

//Host Interface Registers Base Address.
#define PICORV_HIR_REG_BASE (PICORV_SYS_REG_BASE + 16*4)

//Output IRQ system register offset
#define PICORV_SYS_REG_IRQ_OUT 0
//Input IRQ system register offset
#define PICORV_SYS_REG_IRQ_IN 1
//Control system register offsset
#define PICORV_SYS_REG_CTRL 2

//Host Interface register offsets
#define PICORV_HIR_0 0
#define PICORV_HIR_1 1
#define PICORV_HIR_2 2
#define PICORV_HIR_3 3
#define PICORV_HIR_4 4
#define PICORV_HIR_5 5
#define PICORV_HIR_6 6
#define PICORV_HIR_7 7
#define PICORV_HIR_8 8
#define PICORV_HIR_9 9
#define PICORV_HIR_10 10
#define PICORV_HIR_11 11
#define PICORV_HIR_12 12
#define PICORV_HIR_13 13
#define PICORV_HIR_14 14
#define PICORV_HIR_15 15

//Write to System Register
inline void picorv_sys_reg_wr(unsigned reg_offset, unsigned val)
{
	assert(reg_offset <= PICORV_SYS_REG_CTRL);
	((unsigned volatile *)(PICORV_SYS_REG_BASE))[reg_offset] = val;
}

//Read from System Register
inline unsigned picorv_sys_reg_rd(unsigned reg_offset)
{
	assert(reg_offset <= PICORV_SYS_REG_CTRL);
	return ((unsigned volatile *)(PICORV_SYS_REG_BASE))[reg_offset];
}

//Write to Host Interface Register
inline void picorv_hir_reg_wr(unsigned hir_reg_offset, unsigned val)
{
	assert(hir_reg_offset <= PICORV_HIR_15);
	((unsigned volatile *)(PICORV_HIR_REG_BASE))[hir_reg_offset] = val;
}

//Read from Host Interface Register
inline unsigned picorv_hir_reg_rd(unsigned hir_reg_offset)
{
	assert(hir_reg_offset <= PICORV_HIR_15);
	return ((unsigned volatile *)(PICORV_HIR_REG_BASE))[hir_reg_offset];
}

//Load a PicoRV program into Program Memory.
//progLen must be a multiple of 4 and <= PICORV_PROG_SIZE_BYTES.
void picorv_load_program(unsigned char *progData, unsigned progLen);

#ifdef __cplusplus
}
#endif

#endif //PICORV_DMA_HAL_H