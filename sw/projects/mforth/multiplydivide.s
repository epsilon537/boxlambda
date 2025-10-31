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
#  Multiplication, Division and Remainder available as opcodes in RV32IM
#  Rewrite these to get a pure RV32I Forth.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "*" # ( x1 x2 -- x1*x2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  mul x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x02000033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # mul x8, x15, x8

multiply_opcodierung:

  push x1

  addi x9, x9, -CELL
  sc x15, 0(x9)

  pushdaconst reg_tmp1
  call registerliteralkomma

  pop x1
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "um*" # ( u1 u2 -- ud )
um_star:
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  mul x14, x15, x8
  sc x14, 0(x9)
  mulhu x8, x15, x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "m*" # ( n1 n2 -- d )
m_star:
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  mul x14, x15, x8
  sc x14, 0(x9)
  mulh x8, x15, x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "mulhsu" # ( n1 n2 -- n )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  mulhsu x8, x15, x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "/" # ( n1 n2 -- n1/n2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  div x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x02004033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20  # div x8, x15, x8
  j multiply_opcodierung

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "mod" # ( n1 n2 -- n1%n2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  rem x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x02006033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # rem x8, x15, x8
  j multiply_opcodierung

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "u/mod" # ( u1 u2 -- rem quot )
u_divmod:
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  remu x14, x15, x8
  sc x14, 0(x9)
  divu x8, x15, x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "/mod" # ( n1 n2 -- rem quot )
divmod:
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  rem x14, x15, x8
  sc x14, 0(x9)
  div x8, x15, x8
  ret
