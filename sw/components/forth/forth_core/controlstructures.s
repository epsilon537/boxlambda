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

# Jumps, Utilities and Control Structures

jump_too_far:
  writeln "Jump too far"
  j quit

#------------------------------------------------------------------------------
generate_uj_encoding:
#------------------------------------------------------------------------------
  push x1
  call uj_encoding_q
  j 1f

#------------------------------------------------------------------------------
generate_sb_encoding:
#------------------------------------------------------------------------------
  push x1
  call sb_encoding_q

1:popda x15
  beq x15, zero, jump_too_far
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_1, "uj-encoding?" # ( x -- x false | bitmask true )
uj_encoding_q:
#------------------------------------------------------------------------------

  li x15, -524288 # 0xFFF80000 # Range BIT19 to BIT1, but lowest bit is ignored.
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, false

1:# Constant fits in available bits !
  push x1
  call uj_encoding
  pop x1
  j true

#------------------------------------------------------------------------------
  Definition Flag_foldable_1, "sb-encoding?" # ( x -- x false | bitmask true )
sb_encoding_q:
#------------------------------------------------------------------------------

  li x15, -4096 # 0xFFFFF000 # Range BIT12 to BIT1, but lowest bit is ignored.
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, false

1:# Constant fits in available bits !
  push x1
  call sb_encoding
  pop x1
  j true

#------------------------------------------------------------------------------
uj_encoding: # ( addr -- x ) For unconditional jumps
#------------------------------------------------------------------------------
  srli x15, x8,  12
  andi x15, x15, 255
  slli x14, x15,    12

  srli x15,  x8, 11
  andi x15, x15,  1
  slli x15, x15,    20
  or x14, x14, x15

  srli x15,  x8,  1
  andi x15, x15, 1023
  slli x15, x15,    21
  or x14, x14, x15

  srli x15,  x8, 20
  andi x15, x15, 1
  slli x15, x15,   31
  or x8, x14, x15

  ret

#------------------------------------------------------------------------------
sb_encoding: # ( addr -- x ) For conditional jump
#------------------------------------------------------------------------------
  srli x15, x8,  11
  andi x15, x15,  1
  slli x14, x15,    7

  srli x15,  x8,  1
  andi x15, x15, 15
  slli x15, x15,    8
  or x14, x14, x15

  srli x15,  x8,  5
  andi x15, x15, 63
  slli x15, x15,    25
  or x14, x14, x15

  srli x15,  x8, 12
  andi x15, x15, 1
  slli x15, x15,   31
  or x8, x14, x15

  ret

#------------------------------------------------------------------------------
nullprobekomma: # ( -- ) Get TOS into x15 for a following conditional jump.
#------------------------------------------------------------------------------
  push x1

  # Opcodes for "popda x15"
  pushdaconst 0x00000013 | reg_tmp1 << 7  | reg_tos << 15 # mv x15, x8
  call wkomma


  pop x1

#------------------------------------------------------------------------------
dropkomma: # ( -- )
  pushdaaddr drop_jump
  j inlinekomma

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "then"
  # ( Address-for-Jump 2   | Address-for-Jump 5 --)
structure_then:
#------------------------------------------------------------------------------
  push x1

  popda x15
  li x14, 5 # Insert absolute forward jump
  bne x15, x14, 1f

    # Fill in unconditional forward jummp
    # ( jump-space )
    call here
    # ( jump-space jump-target )
    lc x15, 0(x9)
    sub x8, x8, x15

    call generate_uj_encoding
    ori x8, x8, 0x0000006f # jal zero, 0
    # ( jump-space opcode )
    j 2f

1:li x14, 2 # Insert conditional forward jump
  bne x15, x14, structures_dont_match

    # Fill in conditional forward jump
    # ( jump-space )
    call here
    # ( jump-space jump-target )
    lc x15, 0(x9)
    sub x8, x8, x15

    call generate_sb_encoding
    li x15, 0x00000063 | reg_tmp1 << 15 # beq x15, zero, 0
    or x8, x8, x15

2:  # ( jump-space opcode )
    swap
    pop x1
    j kommasomewhere

structures_dont_match:
  writeln "Structures don't match"
  j quit

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "else"
structure_else:
#------------------------------------------------------------------------------
  push x1
  call structure_ahead
  call dswap
  pop x1
  j structure_then

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "if"
structure_if: # ( -- Address-for-Jump 2 )
#------------------------------------------------------------------------------
  push x1

  call nullprobekomma
  call here
  pushdaconst 2 # Strukturerkennung  Structure matching

  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "ahead"
structure_ahead: # ( -- Address-for-Jump 5 )
#------------------------------------------------------------------------------
  push x1

  call here
  pushdaconst 5 # Structure matching

1:call four_allot

  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "repeat"
#------------------------------------------------------------------------------
  push x1
  call structure_again
  pop x1
  j structure_then

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "while"
#------------------------------------------------------------------------------
  push x1
  call structure_if
  pop x1
  j dswap

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "until"
  # ( jump-target 1 -- )                          # Conditional loop
#------------------------------------------------------------------------------
structure_until:
  push x1
  popda x15
  li x14, 1
  bne x15, x14, structures_dont_match

    # Write conditional backward jump

    call nullprobekomma

    call here
    call minus

    call generate_sb_encoding
    li x15, 0x00000063 | reg_tmp1 << 15 # beq x15, zero, 0
    or x8, x8, x15

  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "again"
structure_again:  # ( jump-target 1 -- )           # Unconditional loop
#------------------------------------------------------------------------------
  push x1
  popda x15
  li x14, 1
  bne x15, x14, structures_dont_match

    # Write unconditional backward jump

    call here
    call minus

    call generate_uj_encoding
    ori x8, x8, 0x0000006f # jal zero, 0

1:
  pop x1
  j wkomma

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "begin"
  # ( -- Jump-destination 1 )
#------------------------------------------------------------------------------
structure_begin:
  push x1
  call here
  pushdaconst 1  # Structure matching
  pop x1
  ret

