#    BoxLambda port of Mecrisp Quintus Forth by Ruben Lysens/Epsilon537.
#    Original header below:
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

# Logic.

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "and" # ( x1 x2 -- x1&x2 )
                        # Combines the top two stack elements using bitwise AND.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  and x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:
opcodeentry_and:

  li x14, 0x00007013 | reg_tos << 7 | reg_tos << 15                  # andi x8, x8, ...
  li x15, 0x00007033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # and  x8, x8, x15

  j opcodeentry_signed

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "bic" # ( x1 x2 -- x1&~x2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  inv x8
  and x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  inv x8
  j opcodeentry_and

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "or" # ( x1 x2 -- x1|x2 )
                       # Combines the top two stack elements using bitwise OR.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  or x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x14, 0x00006013 | reg_tos << 7 | reg_tos << 15  # ori x8, x8, ...
  li x15, 0x00006033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # or x8, x8, x15

  j opcodeentry_signed

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "xor" # ( x1 x2 -- x1|x2 )
                        # Combines the top two stack elements using bitwise exclusive-OR.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  xor x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x14, 0x00004013 | reg_tos << 7 | reg_tos << 15                  # xori x8, x8, ...
  li x15, 0x00004033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # xor  x8, x8, x15

  j opcodeentry_signed

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1, "clz" # ( x -- u )
                        # Counts leading zeroes in x.
# -----------------------------------------------------------------------------
  li x15, 32
  beq x8, zero, 2f # If TOS contains 0 we have 32 leading zeros.
  li x15, 0         # No zeros counted yet.

1:blt x8, zero, 2f # Stop if an one will be shifted out.
  sll x8, x8, 1    # Shift TOS one place
  # beq x8, zero, 2f # Stop if register is zero.
  addi x15, x15, 1
  j 1b

2:mv x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "ror" # ( x -- x' )
# -----------------------------------------------------------------------------
  # Rotate right by one bit place
  slli x15, x8, SIGNSHIFT
  srli x8, x8, 1
  or x8, x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "rol" # ( x -- x' )
# -----------------------------------------------------------------------------
  # Rotate left by one bit place
  srli x15, x8, SIGNSHIFT
  slli x8, x8, 1
  or x8, x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "arshift" # ( x n -- x' )
                            # Shifts 'x' right by 'n' bits, shifting in x's MSB.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sra x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x40005013 | reg_tos << 7 | reg_tos << 15 # srai x8, x8, 0

  j opcodeentry_shift

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "rshift" # ( x n -- x' )
                           # Shifts 'x' right by 'n' bits.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  srl x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00005013 | reg_tos << 7 | reg_tos << 15 # srli x8, x8, 0

  j opcodeentry_shift

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodable, "lshift" # ( x n -- x' )
                           # Shifts 'x' left by 'n' bits.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sll x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00001013 | reg_tos << 7 | reg_tos << 15 # slli x8, x8, 0

opcodeentry_shift:

  # The shifts all take up only 5 bits of shift width. Everything else is masked away with $1F ($3F).
  andi x8, x8, SHIFTMASK
  slli x8, x8, 20
  or x8, x8, x15
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "not" # ( x -- ~x )
# -----------------------------------------------------------------------------
  inv x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "invert" # ( x -- ~x )
# -----------------------------------------------------------------------------
  inv x8
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_0|Flag_inline|Flag_noframe, "true" # ( -- -1 )
true:
# -----------------------------------------------------------------------------
  pushdaconst -1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_0|Flag_inline|Flag_noframe, "false" # ( -- -1 )
false:
# -----------------------------------------------------------------------------
  pushdaconst 0
  ret

