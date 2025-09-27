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

# Logikfunktionen
# Logic.

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "and" # ( x1 x2 -- x1&x2 )
                        # Combines the top two stack elements using bitwise AND.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  and x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:
opcodiereinsprung_and:

  .ifdef mipscore

  li x14, 0x30000000 | reg_tos << 21 | reg_tos << 16
  li x15, 0x00000024 | reg_tos << 11 | reg_tos << 21 | reg_tmp1 << 16

opcodiereinsprung_mips_unsigned:

  push x1
  pushdouble x14, x15

  li x15, 0xFFFF0000
  and x14, x8, x15
  bne x14, zero, opcodiereinsprung_lang
  j opcodiereinsprung_kurz

  .else

  li x14, 0x00007013 | reg_tos << 7 | reg_tos << 15                  # andi x8, x8, ...
  li x15, 0x00007033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # and  x8, x8, x15

  j opcodiereinsprung_signed

  .endif

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "bic" # ( x1 x2 -- x1&~x2 )
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  inv x8
  and x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  inv x8
  j opcodiereinsprung_and

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "or" # ( x1 x2 -- x1|x2 )
                       # Combines the top two stack elements using bitwise OR.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  or x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore

  li x14, 0x34000000 | reg_tos << 21 | reg_tos << 16
  li x15, 0x00000025 | reg_tos << 11 | reg_tos << 21 | reg_tmp1 << 16

  j opcodiereinsprung_mips_unsigned

  .else

  li x14, 0x00006013 | reg_tos << 7 | reg_tos << 15  # ori x8, x8, ...
  li x15, 0x00006033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # or x8, x8, x15

  j opcodiereinsprung_signed

  .endif

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "xor" # ( x1 x2 -- x1|x2 )
                        # Combines the top two stack elements using bitwise exclusive-OR.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  xor x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore

  li x14, 0x38000000 | reg_tos << 21 | reg_tos << 16
  li x15, 0x00000026 | reg_tos << 11 | reg_tos << 21 | reg_tmp1 << 16

  j opcodiereinsprung_mips_unsigned

  .else

  li x14, 0x00004013 | reg_tos << 7 | reg_tos << 15                  # xori x8, x8, ...
  li x15, 0x00004033 | reg_tos << 7 | reg_tos << 15 | reg_tmp1 << 20 # xor  x8, x8, x15

  j opcodiereinsprung_signed

  .endif

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
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "ror" # ( x -- x' ) # Um eine Stelle rechts rotieren
# -----------------------------------------------------------------------------
  # Rotate right by one bit place
  slli x15, x8, SIGNSHIFT
  srli x8, x8, 1
  or x8, x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "rol" # ( x -- x' ) # Um eine Stelle links rotieren
# -----------------------------------------------------------------------------
  # Rotate left by one bit place
  srli x15, x8, SIGNSHIFT
  slli x8, x8, 1
  or x8, x8, x15
  ret

.ifdef RV64

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "warshift" # ( x n -- x' )
                            # Shifts 'x' right by 'n' bits, shifting in x's MSB.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sraw x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x4000501b | reg_tos << 7 | reg_tos << 15 # sraiw x8, x8, 0
  andi x8, x8, 0x1F
  j opcodiereinsprung_shift

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "wrshift" # ( x n -- x' )
                           # Shifts 'x' right by 'n' bits.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  srlw x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x0000501b | reg_tos << 7 | reg_tos << 15 # srliw x8, x8, 0
  andi x8, x8, 0x1F
  j opcodiereinsprung_shift

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "wlshift" # ( x n -- x' )
                            # Shifts 'x' right by 'n' bits, shifting in x's MSB.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sllw x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x0000101b | reg_tos << 7 | reg_tos << 15 # slliw x8, x8, 0
  andi x8, x8, 0x1F
  j opcodiereinsprung_shift

.endif


# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "arshift" # ( x n -- x' )
                            # Shifts 'x' right by 'n' bits, shifting in x's MSB.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sra x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x00000003 | reg_tos << 16 | reg_tos << 11
  .else
  li x15, 0x40005013 | reg_tos << 7 | reg_tos << 15 # srai x8, x8, 0
  .endif

  j opcodiereinsprung_shift

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "rshift" # ( x n -- x' )
                           # Shifts 'x' right by 'n' bits.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  srl x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x00000002 | reg_tos << 16 | reg_tos << 11
  .else
  li x15, 0x00005013 | reg_tos << 7 | reg_tos << 15 # srli x8, x8, 0
  .endif

  j opcodiereinsprung_shift

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe|Flag_opcodierbar, "lshift" # ( x n -- x' )
                           # Shifts 'x' left by 'n' bits.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  sll x8, x15, x8
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x00000000 | reg_tos << 16 | reg_tos << 11
  .else
  li x15, 0x00001013 | reg_tos << 7 | reg_tos << 15 # slli x8, x8, 0
  .endif

opcodiereinsprung_shift:

  # Die Schübe nehmen alle nur 5 Bit Schubweite auf. Alles andere wird mit $1F ( $3F ) and wegmaskiert.
  andi x8, x8, SHIFTMASK
  .ifdef mipscore
  slli x8, x8, 6
  .else
  slli x8, x8, 20
  .endif
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
