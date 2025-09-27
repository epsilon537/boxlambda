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

# Sprünge, Helferlein und Kontrollstrukturen
# Jumps, Utilities and Control Structures

jump_too_far:
  writeln "Jump too far"
  j quit

  .ifdef mipscore

#------------------------------------------------------------------------------
generate_16bit_jump_encoding:
#------------------------------------------------------------------------------
  addi x8, x8, -4
  srai x8, x8, 2

  li x15, 0xFFFF8000 # Possible range
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, jump_too_far

1:# Constant fits in available bits !
  andi x8, x8, 0x0000FFFF
  ret

  .else # Tools for RISC-V

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

  .ifndef RV64
  .ifdef compressed_isa

#------------------------------------------------------------------------------
  Definition Flag_foldable_1, "cj-encoding?" # ( x -- x false | bitmask true )
cj_encoding_q:
#------------------------------------------------------------------------------

  li x15, 0xFFFFF800 # Range BIT11 to BIT1, but lowest bit is ignored.
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, false

1:# Constant fits in available bits !
  push x1
  call cj_encoding
  pop x1
  j true

#------------------------------------------------------------------------------
cj_encoding: # ( addr -- x ) For c.jal und c.j
#------------------------------------------------------------------------------

  # [11|4|9:8|10|6|7|3:1|5] %01

  srli x15,  x8, 5
  andi x15, x15, 1
  slli x14, x15,   2

  srli x15,  x8, 1
  andi x15, x15, 7
  slli x15, x15,   3
  or x14, x14, x15

  srli x15,  x8, 7
  andi x15, x15, 1
  slli x15, x15,   6
  or x14, x14, x15

  srli x15,  x8, 6
  andi x15, x15, 1
  slli x15, x15,   7
  or x14, x14, x15

  srli x15,  x8, 10
  andi x15, x15, 1
  slli x15, x15,   8
  or x14, x14, x15

  srli x15,  x8, 8
  andi x15, x15, 3
  slli x15, x15,   9
  or x14, x14, x15

  srli x15,  x8, 4
  andi x15, x15, 1
  slli x15, x15,   11
  or x14, x14, x15

  srli x15,  x8, 11
  andi x15, x15, 1
  slli x15, x15,   12
  or x8, x14, x15

  ret

  .endif # compressed_isa
  .endif # ifndef RV64

  .endif # Tools for RISC-V

#------------------------------------------------------------------------------
nullprobekomma: # ( -- ) Get TOS into x15 for a following conditional jump.
#------------------------------------------------------------------------------
  push x1

  # Opcodes for "popda x15"

  .ifdef mipscore
    pushdaconst 0x00000025 | reg_tmp1 << 11 | reg_tos << 21
    call wkomma
  .else

  .ifdef compressed_isa
    pushdaconst 0x87A2 # c.mv x15, x8
    call hkomma
  .else
    pushdaconst 0x00000013 | reg_tmp1 << 7  | reg_tos << 15 # mv x15, x8
    call wkomma
  .endif

  .endif

  pop x1

#------------------------------------------------------------------------------
dropkomma: # ( -- )
  pushdaaddr drop_einsprung
  j inlinekomma

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "then"
  # ( Adresse-für-Sprung 2 | Adresse-für-Sprung 5 -- )
  # ( Address-for-Jump 2   | Address-for-Jump 5 --)
struktur_then:
#------------------------------------------------------------------------------
  push x1

  popda x15
  li x14, 5 # Unbedingten Vorwärtssprung einfügen
  bne x15, x14, 1f

    # Abschluss unbedingter Vorwärtssprung  Fill in unconditional forward jummp
    # ( Sprunglücke )
    call here
    # ( Sprunglücke Sprungziel )
    lc x15, 0(x9)
    sub x8, x8, x15

    .ifdef mipscore
    call generate_16bit_jump_encoding
    li x15, 0x10000000
    or x8, x8, x15
    .else
    call generate_uj_encoding
    ori x8, x8, 0x0000006f # jal zero, 0
    .endif
    # ( Sprunglücke Opcode )
    j 2f

1:li x14, 2 # Bedingten Vorwärtssprung einfügen
  bne x15, x14, strukturen_passen_nicht

    # Abschluss bedingter Vorwärtssprung Fill in conditional forward jump
    # ( Sprunglücke )
    call here
    # ( Sprunglücke Sprungziel )
    lc x15, 0(x9)
    sub x8, x8, x15

    .ifdef mipscore
    call generate_16bit_jump_encoding
    li x15, 0x10000000 | reg_tmp1 << 21
    or x8, x8, x15
    .else
    call generate_sb_encoding
    li x15, 0x00000063 | reg_tmp1 << 15 # beq x15, zero, 0
    or x8, x8, x15
    .endif

2:  # ( Sprunglücke Opcode )
    swap
    pop x1
    j kommairgendwo

strukturen_passen_nicht:
  writeln "Structures don't match"
  j quit

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "else"
struktur_else:
#------------------------------------------------------------------------------
  push x1
  call struktur_ahead
  call dswap
  pop x1
  j struktur_then

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "if"
struktur_if: # ( -- Adresse-für-Sprung 2 )   ( -- Address-for-Jump 2 )
#------------------------------------------------------------------------------
  push x1

  call nullprobekomma
  call here
  pushdaconst 2 # Strukturerkennung  Structure matching

  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "ahead"
struktur_ahead: # ( -- Adresse-für-Sprung 5 )   ( -- Address-for-Jump 5 )
#------------------------------------------------------------------------------
  push x1

  call here
  pushdaconst 5 # Strukturerkennung  Structure matching

1:call four_allot

  .ifdef mipscore # NOP for branch delay slot
    pushdaconst 0
    call wkomma
  .endif

  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "repeat"
#------------------------------------------------------------------------------
  push x1
  call struktur_again
  pop x1
  j struktur_then

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "while"
#------------------------------------------------------------------------------
  push x1
  call struktur_if
  pop x1
  j dswap

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "until"  # Bedingte Schleife
  # ( Sprungziel 1 -- )                          # Conditional loop
#------------------------------------------------------------------------------
struktur_until:
  push x1
  popda x15
  li x14, 1
  bne x15, x14, strukturen_passen_nicht

    # Write conditional backward jump

    call nullprobekomma

    call here
    call minus

    .ifdef mipscore
    call generate_16bit_jump_encoding
    li x15, 0x10000000 | reg_tmp1 << 21
    or x8, x8, x15
    .else
    call generate_sb_encoding
    li x15, 0x00000063 | reg_tmp1 << 15 # beq x15, zero, 0
    or x8, x8, x15
    .endif

  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "again"  # Endlosschleife
struktur_again:  # ( Sprungziel 1 -- )           # Unconditional loop
#------------------------------------------------------------------------------
  push x1
  popda x15
  li x14, 1
  bne x15, x14, strukturen_passen_nicht

    # Write unconditional backward jump

    call here
    call minus

    .ifdef mipscore
    call generate_16bit_jump_encoding
    li x15, 0x10000000
    or x8, x8, x15
    .else
    call generate_uj_encoding
    ori x8, x8, 0x0000006f # jal zero, 0
    .endif

1:
  .ifdef mipscore # NOP for branch delay slot
    call wkomma
    pushdaconst 0
  .endif

  pop x1
  j wkomma

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "begin"
  # ( -- Sprungziel 1 )  ( -- Jump-destination 1 )
#------------------------------------------------------------------------------
struktur_begin:
  push x1
  call here
  pushdaconst 1  # Strukturerkennung  Structure matching
  pop x1
  ret
