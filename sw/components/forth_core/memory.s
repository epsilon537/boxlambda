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

# Memory access

# -----------------------------------------------------------------------------
  Definition Flag_visible, "move"  # Move some bytes around. This can cope with overlapping memory areas.
move:  # ( Source Destination Count -- )
# -----------------------------------------------------------------------------

  push x10

  popdadouble x14, x15
  # x8: Source address
  # x15: Destination address
  # x14: Count

  # push x1
  # push x15
  # push x14
  # write " Move: "
  # pushda x8
  # call hexdot
  # pushda x15
  # call hexdot
  # pushda x14
  # call hexdot
  # writeln ""
  # pop x14
  # pop x15
  # pop x1

  beq  x14, zero, 3f  # Nothing to do if count is zero.
  beq  x8,  x15,  3f  # If source and destination are the same, nothing to do, too.
  bltu x15,  x8,  2f  # Compare destination with source address to find out which direction to copy.

  # Source < Destination --> Backward move

  add x8, x8, x14
  add x15, x15, x14

1:lbu x10, -1(x8)
  sb  x10, -1(x15)

  addi x8, x8, -1
  addi x15, x15, -1

  addi x14, x14, -1
  bne x14, zero, 1b

  j 3f

  # Source > Destination --> Forward move

2:lbu x10, 0(x8)
  sb  x10, 0(x15)

  addi x8, x8, 1
  addi x15, x15, 1

  addi x14, x14, -1
  bne x14, zero, 2b

3:drop
  pop x10
  ret

# : s1  s" 0123456789ABCDEF" 2dup 2>r   drop    4 + dup 5 + 3 move  2r> ." >" type ." <" ; \ >012345678456CDEF< ok.
# : s2  s" 0123456789ABCDEF" 2dup 2>r   drop    4 + dup 2 - 3 move  2r> ." >" type ." <" ; \ >0145656789ABCDEF< ok.

# -----------------------------------------------------------------------------
  Definition Flag_visible, "fill"  # Fill memory with given byte.
fill:  # ( Destination Count Filling -- )
# -----------------------------------------------------------------------------
  # 6.1.1540 FILL CORE ( c-addr u char -- ) If u is greater than zero, store char in each of u consecutive characters of memory beginning at c-addr.

  popdadouble x14, x15
  # x14: Filling byte
  # x15: Count
  # x8:  Destination

  beq x15, zero, 2f

1:sb x14, 0(x8)
  addi x8, x8, 1
  addi x15, x15, -1
  bne x15, zero, 1b

2:drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "@" # ( 32-addr -- x )
                              # Loads the cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00002003 | reg_tos << 7  | reg_tos << 15  # lw x8, 0(x8)

# -----------------------------------------------------------------------------
opcode_fetch_adressconstant:
# -----------------------------------------------------------------------------
  push_x1_x10

  mv x10, x15
  #  x8: Address to be inserted
  # x10: Opcode

  # Clean up constants

  to_r # Get the top constant.
  addi x13, x13, -1 # Subtract the top one from the number of folding constants.
  call writeconstants # and write all other constants
  r_from # Put the top constant back to edit it later

  # Write intro

  call dup_komma

  # Test whether the constant can also be loaded more quickly:

  li x15, -2048 # 0xFFFFF800
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, 2f

1:# Short variant with only one opcode: Swap register x8 with x0.
  li x15, ~0x000F8000
  and x10, x10, x15
  j 4f

2:# Long variant with 2 opcodes.

  # Correction for the negative sign in the addition
  li x15, 0x800
  and x15, x15, x8
  beq x15, zero, 3f
    li x15, 0x1000
    add x8, x8, x15
3:

  dup
  li x15, -4096 # 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x000000037 | reg_tos << 7  # lui x8, ...
  call wkomma


4:# Just one opcode, or the 2nd part of 2 opcodes

  li x15, 0xFFF  # Imm [11:0]
  and x15, x8, x15
  sll x15, x15, 20

  or x8, x10, x15
  pop_x1_x10
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "!" # ( x 32-addr -- )
# Given a value 'x' and a cell-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
store:
  lc x15, 0(x9)
  sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x14, 0x00002023 | reg_tos  << 20 | reg_tmp2 << 15 # sw x8,  0(x14)
  li x15, 0x00002023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sw x15, 0(x14)

  # Fallthrough:

# -----------------------------------------------------------------------------
opcode_store_address_constant:
# -----------------------------------------------------------------------------

  push_x1_x10

  addi x13, x13, -1
  bne x13, zero, 1f

  # One constant
  mv x10, x14
  call opcode_store_address_constant_intern
  call dropkomma
  j 2f

1:# Two constants
  mv x10, x15

  swap
  pushdaconst reg_tmp1
  call registerliteralkomma

  call opcode_store_address_constant_intern

2:pop_x1_x10
  ret


opcode_store_address_constant_intern:

  push x1 # x10 will already be secured when we arrive here.

  #  x8: Address to be inserted
  # x10: Opcode

  # Test whether the constant can also be loaded more quickly:

  li x15, -2048 # 0xFFFFF800
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, 2f

1:# Short version with only one opcode: Swap register x14 with x0.
  li x15, ~0x000F8000
  and x10, x10, x15
  j 4f

2:# Long variant with 2 opcodes

  # Correction for the negative sign in the addition
  li x15, 0x800
  and x15, x15, x8
  beq x15, zero, 3f
    li x15, 0x1000
    add x8, x8, x15
3:

  dup
  li x15, -4096 # 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x000000037 | reg_tmp2 << 7  # lui x14, ...
  call wkomma


4:# Just one opcode, or the 2nd part of 2 opcodes


  andi x15, x8, 0x1F  # Imm [4:0]
  sll x15, x15, 7
  or x10, x10, x15

  li x15, 0xFE0  # Imm [11:5]
  and x15, x8, x15
  srl x15, x15, 5
  sll x15, x15, 25

  or x8, x10, x15

  pop x1
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_visible, "+!" # ( x 32-addr -- )
                               # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lc x14, 0(x8)
    add x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "h@" # ( 16-addr -- x )
                              # Loads the half-word at 'addr'.
# -----------------------------------------------------------------------------
  lhu x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00005003 | reg_tos << 7  | reg_tos << 15  # lhu x8, 0(x8)
  j opcode_fetch_adressconstant

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "h@signed" # ( 16-addr -- x )
                              # Loads the half-word at 'addr'.
# -----------------------------------------------------------------------------
  lh x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00001003 | reg_tos << 7  | reg_tos << 15  # lh x8, 0(x8)
  j opcode_fetch_adressconstant

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "h!" # ( x 16-addr -- )
# Given a value 'x' and an 16-bit-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
hstore:
  lc x15, 0(x9)
  sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x14, 0x00001023 | reg_tos  << 20 | reg_tmp2 << 15 # sh x8,  0(x14)
  li x15, 0x00001023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sh x15, 0(x14)

  j opcode_store_address_constant


# -----------------------------------------------------------------------------
  Definition Flag_visible, "h+!" # ( x 16-addr -- )
                                # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lhu x14, 0(x8)
    add x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "c@" # ( 8-addr -- x )
                              # Loads the byte at 'addr'.
# -----------------------------------------------------------------------------
  lbu x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00004003 | reg_tos << 7  | reg_tos << 15  # lbu x8, 0(x8)
  j opcode_fetch_adressconstant

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "c@signed" # ( 8-addr -- x )
                              # Loads the byte at 'addr'.
# -----------------------------------------------------------------------------
  lb x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x15, 0x00000003 | reg_tos << 7  | reg_tos << 15  # lb x8, 0(x8)
  j opcode_fetch_adressconstant

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "c!" # ( x 8-addr -- )
# Given a value 'x' and an 8-bit-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  sb x15, 0(x8)
  lc x8, CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcode-Entry:

  li x14, 0x00000023 | reg_tos  << 20 | reg_tmp2 << 15 # sb x8,  0(x14)
  li x15, 0x00000023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sb x15, 0(x14)

  j opcode_store_address_constant


# -----------------------------------------------------------------------------
  Definition Flag_visible, "c+!" # ( x 8-addr -- )
                               # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lbu x14, 0(x8)
    add x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bis!" # ( x 32-addr -- )  Set bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lc x14, 0(x8)
    or x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bic!" # ( x 32-addr -- )  Clear bits
  # Deletes the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lc x14, 0(x8)
    and x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "xor!" # ( x 32-addr -- )  Toggle bits
  # Changes the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lc x14, 0(x8)
    xor x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bit@" # ( x 32-addr -- Flag )  Check bits
  # Checks whether bits are set in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hbis!" # ( x 16-addr -- )  Set bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lhu x14, 0(x8)
    or x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hbic!" # ( x 16-addr -- )  Clear bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lhu x14, 0(x8)
    and x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hxor!" # ( x 16-addr -- )  Toggle bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lhu x14, 0(x8)
    xor x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hbit@" # ( x 16-addr -- Flag )  Check bits
  # Checks whether bits are set in the memory location
# -----------------------------------------------------------------------------
  lhu x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cbis!" # ( x 8-addr -- )  Set bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lbu x14, 0(x8)
    or x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cbic!" # ( x 8-addr -- )  Clear bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lbu x14, 0(x8)
    and x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cxor!" # ( x 8-addr -- )  Toggle bits
  # Sets the bits in the memory location
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lbu x14, 0(x8)
    xor x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cbit@" # ( x 8-addr -- Flag )  Check bits
  # Checks whether bits are set in the memory location
# -----------------------------------------------------------------------------
  lbu x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret

