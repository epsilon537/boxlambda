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

# Kleine Rechenmeister
# Small calculations

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_opcodierbar|Flag_inline|Flag_noframe, "+" # ( x1 x2 -- x1+x2 )
                      # Adds x1 and x2.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  add x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

opcodiereinsprung_plus:

  .ifdef mipscore

  li x14, 0x24000000 | reg_tos << 21 | reg_tos << 16
  li x15, 0x00000021 | reg_tos << 11 | reg_tos << 21 | reg_tmp1 << 16

  .else

  li x14, 0x00000013 | reg_tos << 7  | reg_tos << 15                  # addi x8, x8, ...
  li x15, 0x00000033 | reg_tos << 7  | reg_tos << 15 | reg_tmp1 << 20 # add  x8, x8, x15

  .endif

opcodiereinsprung_signed:

  push x1
  pushdouble x14, x15

  # Probe, ob die Konstante in einen einzigen Opcode passt:
  .ifdef mipscore
  li x15, 0xFFFF8000
  .else
  li x15, -2048 # 0xFFFFF800
  .endif
  and x14, x8, x15
  beq x14, x15,  opcodiereinsprung_kurz
  bne x14, zero, opcodiereinsprung_lang

opcodiereinsprung_kurz: # Kurze Variante mit nur einem Opcode.

  .ifdef mipscore
    andi x8, x8, 0xFFFF
  .else
    sll x8, x8, 20
  .endif

  lc x15, CELL(sp)
  addi sp, sp, 2*CELL

  or  x8, x8, x15
  j 3f

opcodiereinsprung_lang: # Lange Variante mit zwei Opcodes.

  pushdaconst reg_tmp1
  call registerliteralkomma

  pushdatos
  lc x8, 0(sp)
  addi sp, sp, 2*CELL

3:pop x1
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_opcodierbar|Flag_inline|Flag_noframe, "-" # ( x1 x2 -- x1-x2 )
                      # Subtracts x2 from x1.
# -----------------------------------------------------------------------------
minus:
  lc x15, 0(x9)
  addi x9, x9, CELL
  sub x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  inv x8
  addi x8, x8, 1
  j opcodiereinsprung_plus

.ifdef RV64

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_opcodierbar|Flag_inline|Flag_noframe, "w+" # ( x1 x2 -- x1-x2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  addw x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

opcodiereinsprung_wplus:

  li x14, 0x0000001B | reg_tos << 7  | reg_tos << 15                  # addiw x8, x8, ...
  li x15, 0x0000003B | reg_tos << 7  | reg_tos << 15 | reg_tmp1 << 20 # addw  x8, x8, x15

  j opcodiereinsprung_signed

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_opcodierbar|Flag_inline|Flag_noframe, "w-" # ( x1 x2 -- x1-x2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  subw x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  inv x8
  addi x8, x8, 1
  j opcodiereinsprung_wplus

.endif

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_opcodierbar|Flag_inline|Flag_noframe, "slt" # ( x1 x2 -- 0 | 1 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  slt x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore

    li x14, 0x28000000 | reg_tos << 21 | reg_tos << 16
    li x15, 0x0000002A | reg_tos << 11 | reg_tos << 21 | reg_tmp1 << 16

  .else

    li x14, 0x00002013 | reg_tos << 7  | reg_tos << 15                  # slti x8, x8, ...
    li x15, 0x00002033 | reg_tos << 7  | reg_tos << 15 | reg_tmp1 << 20 # slt  x8, x8, x15

  .endif

  j opcodiereinsprung_signed

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_opcodierbar|Flag_inline|Flag_noframe, "sltu" # ( x1 x2 -- 0 | 1 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sltu x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore

    li x14, 0x2C000000 | reg_tos << 21 | reg_tos << 16
    li x15, 0x0000002B | reg_tos << 11 | reg_tos << 21 | reg_tmp1 << 16

  .else

    li x14, 0x00003013 | reg_tos << 7  | reg_tos << 15                  # sltiu x8, x8, ...
    li x15, 0x00003033 | reg_tos << 7  | reg_tos << 15 | reg_tmp1 << 20 # sltu  x8, x8, x15

  .endif

  j opcodiereinsprung_signed

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "1-" # ( u -- u-1 )
                      # Subtracts one from the cell on top of the stack.
# -----------------------------------------------------------------------------
  addi x8, x8, -1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "1+" # ( u -- u+1 )
                       # Adds one to the cell on top of the stack.
# -----------------------------------------------------------------------------
  addi x8, x8, 1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "2-" # ( u -- u-1 )
                      # Subtracts two from the cell on top of the stack.
# -----------------------------------------------------------------------------
  addi x8, x8, -2
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "2+" # ( u -- u+1 )
                       # Adds two to the cell on top of the stack.
# -----------------------------------------------------------------------------
  addi x8, x8, 2
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "cell+" # ( x -- x+4 )
# -----------------------------------------------------------------------------
  addi x8, x8, CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "negate" # ( n1 -- -n1 )
# -----------------------------------------------------------------------------
  sub x8, zero, x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "shr" # ( x -- x' ) # Um eine Stelle rechts schieben
# -----------------------------------------------------------------------------
  srli x8, x8, 1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "shl" # ( x -- x' ) # Um eine Stelle links schieben
# -----------------------------------------------------------------------------
  slli x8, x8, 1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "2*" # ( n -- n*2 )
# -----------------------------------------------------------------------------
  add x8, x8, x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "cells" # ( x -- 4*x )
# -----------------------------------------------------------------------------
  slli x8, x8, CELLSHIFT
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "2/" # ( n -- n/2 )
# -----------------------------------------------------------------------------
  srai x8, x8, 1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "abs" # ( n1 -- |n1| )
# -----------------------------------------------------------------------------
  srai x15, x8, SIGNSHIFT
  add x8, x8, x15
  xor x8, x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "even" # ( x -- x' )
# -----------------------------------------------------------------------------
  andi x15, x8, 1
  add x8, x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "base" # ( -- addr )
  CoreVariable base
# -----------------------------------------------------------------------------
  pushdatos
  laf x8, base
  ret
  .varinit 10

# -----------------------------------------------------------------------------
  Definition Flag_visible, "binary" # ( -- )
# -----------------------------------------------------------------------------
  li x15, 2
  j 1f

# -----------------------------------------------------------------------------
  Definition Flag_visible, "decimal" # ( -- )
# -----------------------------------------------------------------------------
  li x15, 10
  j 1f

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hex" # ( -- )
# -----------------------------------------------------------------------------
  li x15, 16
1:laf x14, base
  sc x15, 0(x14)
  ret
