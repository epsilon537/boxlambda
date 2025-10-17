#
#    Mecrisp-Quintus - A native code Forth implementation for RISC-V
#    Copyright (C) 2018  Matthias Koch
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# -----------------------------------------------------------------------------
# Macros for cell size to switch between 32 bit and 64 bit
# -----------------------------------------------------------------------------

.ifdef RV64

  .equ CELL, 8
  .equ CELLSHIFT, 3
  .equ SIGNSHIFT, 63
  .equ SHIFTMASK, 0x3F
  .equ CELLBITS, 64

  .macro lc op1 op2
    ld \op1, \op2
  .endm

  .macro sc op1 op2
    sd \op1, \op2
  .endm

  .macro .varinit val
    .dword \val
  .endm

.else

  .equ CELL, 4
  .equ CELLSHIFT, 2
  .equ SIGNSHIFT, 31
  .equ SHIFTMASK, 0x1F
  .equ CELLBITS, 32

  .macro lc op1 op2
    lw \op1, \op2
  .endm

  .macro sc op1 op2
    sw \op1, \op2
  .endm

  .macro lwu op1 op2
    lw \op1, \op2
  .endm

  .macro .varinit val
    .word \val
  .endm

.endif

# -----------------------------------------------------------------------------
# Bit-position equates (for setting or clearing a single bit)
# -----------------------------------------------------------------------------

  .equ  BIT0,    0x00000001
  .equ  BIT1,    0x00000002
  .equ  BIT2,    0x00000004
  .equ  BIT3,    0x00000008
  .equ  BIT4,    0x00000010
  .equ  BIT5,    0x00000020
  .equ  BIT6,    0x00000040
  .equ  BIT7,    0x00000080
  .equ  BIT8,    0x00000100
  .equ  BIT9,    0x00000200
  .equ  BIT10,   0x00000400
  .equ  BIT11,   0x00000800
  .equ  BIT12,   0x00001000
  .equ  BIT13,   0x00002000
  .equ  BIT14,   0x00004000
  .equ  BIT15,   0x00008000
  .equ  BIT16,   0x00010000
  .equ  BIT17,   0x00020000
  .equ  BIT18,   0x00040000
  .equ  BIT19,   0x00080000
  .equ  BIT20,   0x00100000
  .equ  BIT21,   0x00200000
  .equ  BIT22,   0x00400000
  .equ  BIT23,   0x00800000
  .equ  BIT24,   0x01000000
  .equ  BIT25,   0x02000000
  .equ  BIT26,   0x04000000
  .equ  BIT27,   0x08000000
  .equ  BIT28,   0x10000000
  .equ  BIT29,   0x20000000
  .equ  BIT30,   0x40000000
  .equ  BIT31,   0x80000000

# -----------------------------------------------------------------------------
# Register definitions for more readable opcode assembly
# -----------------------------------------------------------------------------

.equ reg_loop_index, 3
.equ reg_loop_limit, 4

.equ reg_tos, 8
.equ reg_psp, 9

.equ reg_tmp1, 15
.equ reg_tmp2, 14

# -----------------------------------------------------------------------------
# Macros for return stack and data stack
# -----------------------------------------------------------------------------

.macro push register
  addi sp, sp, -CELL
  sc \register, 0*CELL(sp)
.endm

.macro pop register
  lc \register, 0*CELL(sp)
  addi sp, sp, CELL
.endm


.macro pushdouble register1 register2
  addi sp, sp, -2*CELL
  sc \register1, 1*CELL(sp)
  sc \register2, 0*CELL(sp)
.endm

.macro popdouble register1 register2
  lc \register1, 0*CELL(sp)
  lc \register2, 1*CELL(sp)
  addi sp, sp, 2*CELL
.endm


.macro push_x1_x10
  addi sp, sp, -2*CELL
  sc x1,  1*CELL(sp)
  sc x10, 0*CELL(sp)
.endm
.macro pop_x1_x10
  lc x1,  1*CELL(sp)
  lc x10, 0*CELL(sp)
  addi sp, sp, 2*CELL
.endm


.macro push_x10_x11
  addi sp, sp, -2*CELL
  sc x10, 1*CELL(sp)
  sc x11, 0*CELL(sp)
.endm
.macro pop_x10_x11
  lc x10, 1*CELL(sp)
  lc x11, 0*CELL(sp)
  addi sp, sp, 2*CELL
.endm


.macro push_x1_x10_x11
  addi sp, sp, -3*CELL
  sc x1,  2*CELL(sp)
  sc x10, 1*CELL(sp)
  sc x11, 0*CELL(sp)
.endm
.macro pop_x1_x10_x11
  lc x1,  2*CELL(sp)
  lc x10, 1*CELL(sp)
  lc x11, 0*CELL(sp)
  addi sp, sp, 3*CELL
.endm

.macro push_x1_x3_x4
  addi sp, sp, -3*CELL
  sc x1,  2*CELL(sp)
  sc x3, 1*CELL(sp)
  sc x4, 0*CELL(sp)
.endm
.macro pop_x1_x3_x4
  lc x1,  2*CELL(sp)
  lc x3, 1*CELL(sp)
  lc x4, 0*CELL(sp)
  addi sp, sp, 3*CELL
.endm

.macro push_x10_x12
  addi sp, sp, -3*CELL
  sc x10, 2*CELL(sp)
  sc x11, 1*CELL(sp)
  sc x12, 0*CELL(sp)
.endm
.macro pop_x10_x12
  lc x10, 2*CELL(sp)
  lc x11, 1*CELL(sp)
  lc x12, 0*CELL(sp)
  addi sp, sp, 3*CELL
.endm


.macro push_x1_x10_x12
  addi sp, sp, -4*CELL
  sc x1,  3*CELL(sp)
  sc x10, 2*CELL(sp)
  sc x11, 1*CELL(sp)
  sc x12, 0*CELL(sp)
.endm
.macro pop_x1_x10_x12
  lc x1,  3*CELL(sp)
  lc x10, 2*CELL(sp)
  lc x11, 1*CELL(sp)
  lc x12, 0*CELL(sp)
  addi sp, sp, 4*CELL
.endm


.macro push_x10_x13
  addi sp, sp, -4*CELL
  sc x10, 3*CELL(sp)
  sc x11, 2*CELL(sp)
  sc x12, 1*CELL(sp)
  sc x13, 0*CELL(sp)
.endm
.macro pop_x10_x13
  lc x10, 3*CELL(sp)
  lc x11, 2*CELL(sp)
  lc x12, 1*CELL(sp)
  lc x13, 0*CELL(sp)
  addi sp, sp, 4*CELL
.endm


.macro push_x1_x10_x13
  addi sp, sp, -5*CELL
  sc x1,  4*CELL(sp)
  sc x10, 3*CELL(sp)
  sc x11, 2*CELL(sp)
  sc x12, 1*CELL(sp)
  sc x13, 0*CELL(sp)
.endm
.macro pop_x1_x10_x13
  lc x1,  4*CELL(sp)
  lc x10, 3*CELL(sp)
  lc x11, 2*CELL(sp)
  lc x12, 1*CELL(sp)
  lc x13, 0*CELL(sp)
  addi sp, sp, 5*CELL
.endm

.macro push_x3_x4_x5_x6_x7
  addi sp, sp, -5*CELL
  sc x3, 4*CELL(sp)
  sc x4, 3*CELL(sp)
  sc x5, 2*CELL(sp)
  sc x6, 1*CELL(sp)
  sc x7, 0*CELL(sp)
.endm
.macro pop_x3_x4_x5_x6_x7
  lc x3, 4*CELL(sp)
  lc x4, 3*CELL(sp)
  lc x5, 2*CELL(sp)
  lc x6, 1*CELL(sp)
  lc x7, 0*CELL(sp)
  addi sp, sp, 5*CELL
.endm


.macro push_x1_x5_x6_x10_x13
  addi sp, sp, -7*CELL
  sc x1,  6*CELL(sp)
  sc x5,  5*CELL(sp)
  sc x6,  4*CELL(sp)
  sc x10, 3*CELL(sp)
  sc x11, 2*CELL(sp)
  sc x12, 1*CELL(sp)
  sc x13, 0*CELL(sp)
.endm
.macro pop_x1_x5_x6_x10_x13
  lc x1,  6*CELL(sp)
  lc x5,  5*CELL(sp)
  lc x6,  4*CELL(sp)
  lc x10, 3*CELL(sp)
  lc x11, 2*CELL(sp)
  lc x12, 1*CELL(sp)
  lc x13, 0*CELL(sp)
  addi sp, sp, 7*CELL
.endm

.macro push_x1_x5_x6_x7_x10_x13
  addi sp, sp, -8*CELL
  sc x1,  7*CELL(sp)
  sc x5,  6*CELL(sp)
  sc x6,  5*CELL(sp)
  sc x7,  4*CELL(sp)
  sc x10, 3*CELL(sp)
  sc x11, 2*CELL(sp)
  sc x12, 1*CELL(sp)
  sc x13, 0*CELL(sp)
.endm
.macro pop_x1_x5_x6_x7_x10_x13
  lc x1,  7*CELL(sp)
  lc x5,  6*CELL(sp)
  lc x6,  5*CELL(sp)
  lc x7,  4*CELL(sp)
  lc x10, 3*CELL(sp)
  lc x11, 2*CELL(sp)
  lc x12, 1*CELL(sp)
  lc x13, 0*CELL(sp)
  addi sp, sp, 8*CELL
.endm

.macro push_x1_x3_x5_x10_x13
  addi sp, sp, -8*CELL
  sc x1,  7*CELL(sp)
  sc x3,  6*CELL(sp)
  sc x4,  5*CELL(sp)
  sc x5,  4*CELL(sp)
  sc x10, 3*CELL(sp)
  sc x11, 2*CELL(sp)
  sc x12, 1*CELL(sp)
  sc x13, 0*CELL(sp)
.endm

.macro pop_x1_x3_x5_x10_x13
  lc x1,  7*CELL(sp)
  lc x3,  6*CELL(sp)
  lc x4,  5*CELL(sp)
  lc x5,  4*CELL(sp)
  lc x10, 3*CELL(sp)
  lc x11, 2*CELL(sp)
  lc x12, 1*CELL(sp)
  lc x13, 0*CELL(sp)
  addi sp, sp, 8*CELL
.endm

.macro pushdatos
  addi x9, x9, -CELL
  sc x8, 0(x9)
.endm

.macro dup
  addi x9, x9, -CELL
  sc x8, 0(x9)
.endm

.macro over
  addi x9, x9, -CELL
  sc x8,    0(x9)
  lc x8, CELL(x9)
.endm

.macro ddup # l(0) h(x8) -- l(8) h(4) l(0) h(x8)
  lc x15, 0(x9)
  addi x9, x9, -2*CELL
  sc x8, CELL(x9)
  sc x15,   0(x9)
.endm

.macro swap
  mv x15, x8
  lc x8,  0(x9)
  sc x15, 0(x9)
.endm

.macro drop
  lc x8, 0(x9)
  addi x9, x9, CELL
.endm

.macro nip
  addi x9, x9, CELL
.endm

.macro ddrop
  lc x8, CELL(x9)
  addi x9, x9, 2*CELL
.endm

.macro to_r
  push x8
  drop
.endm

.macro r_from
  pushdatos
  pop x8
.endm

.macro to_r_2
  addi sp, sp, -2*CELL
  sc x8,    0(sp)
  lc x8,    0(x9)
  sc x8, CELL(sp)
  ddrop
.endm

.macro r_from_2
  addi x9, x9, -2*CELL
  sc x8, CELL(x9)

  lc x8, CELL(sp)
  sc x8,    0(x9)
  lc x8,    0(sp)

  addi sp, sp, 2*CELL
.endm

.macro r_fetch_2
  addi x9, x9, -2*CELL
  sc x8, CELL(x9)

  lc x8, CELL(sp)
  sc x8,    0(x9)
  lc x8,    0(sp)
.endm

.macro r_drop_2
  addi sp, sp, 2*CELL
.endm

.macro pushdaconst constant # Push constant on Datastack
  pushdatos
  li x8, \constant
.endm


.ifdef within_os

# Within OS: Forth dictionary points are handled by linker

    .macro laf register, address
      la \register, \address
    .endm

    .macro pushdaaddrf address # Push address constant on Datastack
      pushdatos
      la x8, \address
    .endm

.else

# Embedded: Forth dictionary points are handled by dictionary structure, which appear as constants to the assembler

    .macro laf register, address
      la \register, \address
    .endm

    .macro pushdaaddrf address # Push address constant on Datastack
      pushdatos
      la x8, \address
    .endm

.endif

.macro pushdaaddr address # Push address constant on Datastack
  pushdatos
  la x8, \address
.endm

.macro pushda register # Push register on Datastack
  pushdatos
  mv x8, \register
.endm

.macro popda register # Pop register from Datastack
  mv \register, x8
  drop
.endm

.macro popdanos register # Pop register from next element on Datastack
  lc \register, 0(x9)
  addi x9, x9, CELL
.endm

.macro pushdadouble register1 register2 # Push register on Datastack
  addi x9, x9, -2*CELL
  sc x8,         1*CELL(x9)
  sc \register1, 0*CELL(x9)
  mv x8, \register2
.endm

.macro popdadouble register1 register2 # Pop register from Datastack
  mv \register1, x8
  lc \register2, 0*CELL(x9)
  lc x8,         1*CELL(x9)
  addi x9, x9, 2*CELL
.endm

.macro popdatriple register1 register2 register3 # Pop register from Datastack
  mv \register1, x8
  lc \register2, 0*CELL(x9)
  lc \register3, 1*CELL(x9)
  lc x8,         2*CELL(x9)
  addi x9, x9, 3*CELL
.endm

.macro inv register
  .ifdef mipscore
  nor \register, \register, zero
  .else
  xori \register, \register, -1
  .endif
.endm
