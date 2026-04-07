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

# Counting loops

#------------------------------------------------------------------------------
  Definition Flag_inline, "k" # Third loop index
#------------------------------------------------------------------------------
  # Returnstack ( Limit Index Limit Index )
  pushdatos
  lc x8, 2*CELL(sp)
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "j" # Second loop index
#------------------------------------------------------------------------------
  # Returnstack ( Limit Index )
  pushdatos
  lc x8, 0(sp)
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "i" # Innermost loop index
#------------------------------------------------------------------------------
  # Returnstack ( )
  pushda x3
  ret


/* Some tests

: table   cr 11 1 do
                    i 8 = if leave then
                    11 1 do
                           i j * . space
                           j 5 = if leave then
                           j 2 = if leave then
                         loop
                    cr
                  loop ;

: stars 0 ?do [char] * emit loop ;
: stars5 0 ?do [char] * emit   i 5 = if leave then loop ;

: table   cr 11 1 do 11 1 do i j * . space loop cr loop ;
: table   cr 11 1 do [ .s ] 11 1 do [ .s ] i j * . space loop [ .s ] cr loop [ .s ] ;
: table  cr 11 1 do i 8 = if leave then 11 1 do  i j * . space j 5 = if leave then  j 2 = if leave then loop cr loop ;

*/

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "leave" # Terminates loop immediately.
  # ( ... AlterLeavePointer 0 jump-destination 3 ... )
  # --
  # ( ... AlterLeavePointer forward-jump-dest{or-1-JZ} jump-destination 3 ... )

  # ( ... OldLeavePointer 0 Target 3 ... )
  # --
  # ( ... OldLeavePointer Forward-Jump-Target{or-1-JZ} NumberofJumps Target 3 ... )
#------------------------------------------------------------------------------
  # LeavePointer points to the location which counts the number of jumps that have to be inserted later.
  # Leave moves all elements of stack further, inserts address for jump opcode, increments counter and allots space.

  push_x1_x10_x11

  # Make a hole in datastack at the location the leavepointer points to for inserting the new location a jump opcode has to be patched in later
  # by moving all other elements further one place in datastack.

  laf x10, leavepointer
  lc x11, 0(x10) # The pointed-to location = content of the variable Leavepointer

  mv x14, x9 # Old stackpointer
  addi x9, x9, -CELL # One more element on stack after this

1:# Space shift loop
  lc x15, 0(x14)
  sc x15, -CELL(x14)
  addi x14, x14, CELL
  bne x14, x11, 1b # x11 contains the end position

# Push twice, because two elements are to be inserted for structure-then.

  mv x14, x9 # Old stackpointer
  addi x9, x9, -CELL # One more element on stack after this

1:# Space shift loop
  lc x15, 0(x14)
  sc x15, -CELL(x14)
  addi x14, x14, CELL
  bne x14, x11, 1b # x11 contains the end position

  # Increment the number of jumps to be filled in later

  call here
  call four_allot

  popda x14 # The created-space address

  # Insert the address of location for jump opcode in datastack
  addi x11, x11, -CELL
  sc x14, 0(x11) # Insert gap address

  addi x11, x11, -CELL  # Continue walk toward the top of the stack
  li x15, 5 # Structure recognition for unconditional jump
  sc x15, 0(x11) # Insert gap address

  # Update leavepointer
  sc x11, 0(x10)

  # Increment counter for number of jumps to be generated later

  lc x15, -CELL(x11)  # Copy jump counter from stack
  addi x15, x15, 1    # Increase the jump counter by one and write it back.
  sc x15, -CELL(x11)

  pop_x1_x10_x11
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "(do)"
#------------------------------------------------------------------------------
  pushdouble x4, x3
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "unloop"
unloop:                           # Remove loop structure from returnstack
#------------------------------------------------------------------------------
  popdouble x3, x4 # Fetch back old loop values
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "+loop"
  # Usage: ( Increment -- ).
  # ( OldLeavePointer ... NumberofJumps Target 3 -- )
#------------------------------------------------------------------------------
  li x15, 3  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, structures_dont_match
  drop

  push_x1_x10

  pushdaaddr plusloop_opcodes
  call callkomma

  li x10, 0x00000063 | reg_tmp1 << 15 # beq x15, zero, 0

  j loop_intern

#------------------------------------------------------------------------------

plusloop_opcodes:

  blt x8, zero, 1f

  # Positive increment:
  sub x14, x3, x4 # Index-Limit

    add x3, x3, x8
    drop

  sub x15, x3, x4 # Index+Inc-Limit

  sltu x15, x15, x14
  ret

1:# Negative increment:
  sub x14, x3, x4 # Index-Limit

    add x3, x3, x8
    drop

  sub x15, x3, x4 # Index+Inc-Limit

  sltu x15, x14, x15
  ret


#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "loop"
  # Usage: ( -- ).
  # ( OldLeavePointer ... NumberofJumps Target 3 -- )
#------------------------------------------------------------------------------
  li x15, 3  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, structures_dont_match
  drop

  push_x1_x10

  pushdaconst 0x00100013 | reg_loop_index <<  7 | reg_loop_index << 15 # addi x3, x3, 1 # Increment index.

  call wkomma

  li x10, 0x00001063 | reg_loop_index << 15 | reg_loop_limit << 20 # bne x3, x4, 0

loop_intern:

  call here
  call minus

  call generate_sb_encoding
  or x8, x8, x10

  call wkomma

  popda x10
1:beq x10, zero, 2f
  call structure_then
  addi x10, x10, -1
  j 1b

2:pushdaaddr unloop
  call inlinekomma

  laf x14, leavepointer
  sc x8, 0(x14)         # Fetch back old leavepointer for next loop layer.
  drop

  pop_x1_x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "do"
  # Usage: ( Limit Index -- ).
  # ( -- OldLeavePointer 0 Target 3 )
#------------------------------------------------------------------------------

  # LeavePointer points to the location which counts the number of jumps that have to be inserted later.
  # Leave moves all elements of stack further, inserts address for jump opcode, increments counter and allots space.

  push x1

  call inline_do_opcodes

  laf x14, leavepointer

  pushdatos
  lc x8, 0(x14) # Alten Leavepointer sichern  Save old leavepointer

  pushdaconst 0
  sc x9, 0(x14) # Save current position on datastack

do_intern_finish:
  call here               # Prepare loop jump back to the beginning
  pushdaconst 3           # Structure matching
  pop x1
  ret


inline_do_opcodes:
  pushdaaddr do_opcodes
  j inlinekomma

do_opcodes:
  pushdouble x4, x3
  popdadouble x3, x4
  ret  # End for inline,

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "?do"
  # Usage: ( Limit Index -- ).
  # ( -- OldLeavePointer Forward-Jump-Target 1 Target 3 )
  # This loop terminates immediately if Limit=Index.
#------------------------------------------------------------------------------
  push x1

  call inline_do_opcodes

  pushdaconst 0x40000033 | reg_tmp1 <<  7 | reg_loop_index << 15 | reg_loop_limit << 20 # sub x15, x3, x4
  call wkomma

  laf x14, leavepointer
  pushdatos
  lc x8, 0(x14) # Save old leavepointer

    call here
    call four_allot

    pushdaconst 2           # Structure matching

  pushdaconst 1

  laf x14, leavepointer
  sc x9, 0(x14) # Save current position on datastack

  j do_intern_finish
