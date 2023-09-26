/*This header file contains useful definitions for .picoasm assembly files.*/

/*Starting address of the Host Interface Register (HIR) range*/
.equ HIR_REGS_BASE_ADDR, 0x10003040
/*End sentinel, pointing right past the last GP register.*/
.equ HIR_REGS_END_ADDR, 0x10003080

/*HIR register offsets*/
.equ HIR0, 0
.equ HIR1, 4
.equ HIR2, 8
.equ HIR3, 12
.equ HIR4, 16
.equ HIR5, 20
.equ HIR6, 24
.equ HIR7, 28
.equ HIR8, 32
.equ HIR9, 36
.equ HIR10, 40
.equ HIR11, 44
.equ HIR12, 48
.equ HIR13, 52
.equ HIR14, 56
.equ HIR15, 60

/*Starting address of the System Registers*/
.equ SYS_REG_BASE, 0x10003000
/*IRQ-in register offset*/
.equ IRQ_IN, 4
/*IRQ-out register offset*/
.equ IRQ_OUT, 0
