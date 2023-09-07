#ifndef PRAXOS_REGS_H
#define _REGS_H

/*
 * Praxos DMA hardware access layer.
 */
#define PRAXOS_BASE 0x10000100

#define PRAXOS_IRQ_OUT 0
#define PRAXOS_IRQ_IN 1
#define PRAXOS_PM_DATA_LO 2
#define PRAXOS_PM_DATA_HI 3
#define PRAXOS_PM_ADDR 4
#define PRAXOS_PM_WR 5
#define PRAXOS_CTRL 6
#define PRAXOS_GP_BASE 16

inline void praxos_sys_reg_wr(unsigned reg_offset, unsigned val)
{
	((unsigned volatile *)(PRAXOS_BASE))[reg_offset] = val;
}

inline unsigned praxos_sys_reg_rd(unsigned reg_offset)
{
	return ((unsigned volatile *)(PRAXOS_BASE))[reg_offset];
}

inline void praxos_gp_reg_wr(unsigned gp_reg_offset, unsigned val)
{
	((unsigned volatile *)(PRAXOS_BASE))[PRAXOS_GP_BASE + gp_reg_offset] = val;
}

inline unsigned praxos_gp_reg_rd(unsigned gp_reg_offset)
{
	return ((unsigned volatile *)(PRAXOS_BASE))[PRAXOS_GP_BASE + gp_reg_offset];
}
#endif //PRAXOS_REGS_H