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

# -----------------------------------------------------------------------------
insert_jalrx8: # ( target-opcode-location -- )
# -----------------------------------------------------------------------------
  push_x1_x10

  popda x10 # Opcodelücke

  # Correction for negative sign
  li x15, 0x800
  and x15, x15, x8
  beq x15, zero, 1f
    li x15, 0x00001000
    add x8, x8, x15
1:

  dup
  li x15, 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x00000037 | reg_tmp1 << 7 # lui x15, ...

  pushda x10
  call kommasomewhere

  slli x8, x8, 20
  li x15, 0x00000067 | reg_tos << 7 | reg_tmp1 << 15 # jalr x8, x15, 0
  or x8, x8, x15

  pushda x10
  addi x8, x8, 4
  pop_x1_x10
  j kommasomewhere

# -----------------------------------------------------------------------------
  Definition Flag_inline, "does>"
does: # Gives freshly defined word a special action.
      # Has to be used together with <builds !
# -----------------------------------------------------------------------------
    # At the place where does> is used, a jump to dodoes is inserted and
    # after that a R> to put the address of the definition entering the does>-part
    # on datastack. This is a very special implementation !

  pushdatos
  # Perform this call with absolute addressing.
  lui x15, %hi(dodoes)
  jalr x8, x15, %lo(dodoes)

  ret # Very important as delimiter as does> itself is inline.

dodoes:
  # On the stack: ( Address-to-call R: Return-of-dodoes-itself )
  # Now insert a call into the latest definition which was hopefully prepared by <builds

  # Save and change dictionary pointer to insert a call sequence:

  push x1

  pushdaaddrf Entrypoint
  lc x8, 0(x8)

  addi x8, x8, 16   # Skip pop x1 and pushdatos opcodes

  call insert_jalrx8

  call smudge

  addi sp, sp, CELL # Skip one return layer
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "<builds"
builds: # Brother of does> that creates a new definition and leaves space to insert a call instruction later.
#------------------------------------------------------------------------------
  push x1
  call create # Create new empty definition
  call push_x1_komma # Write opcodes for push x1

  call dup_komma

  # Space to fit the lui/jalr pair.
  pushdaconst 2*4 # A call instruction or its preparation will go here - but I don't know its target address for now.
  call allot # Make a hole to insert the destination later.

  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "create" # ANS-Create with default action.
#------------------------------------------------------------------------------
  push x1
  call builds
  # Copy of the inline-code of does>

  pushdatos
  # Perform this call with absolute addressing.
  lui x15, %hi(dodoes)
  jalr x8, x15, %lo(dodoes)

  pop x1
  ret

