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
  Definition Flag_visible, "risc-v"
# -----------------------------------------------------------------------------
  push x1
  welcome " for RISC-V by Matthias Koch"
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hex."
hexdot: # ( u -- ) Print an unsigned number in Base 16, independent of number subsystem.
# -----------------------------------------------------------------------------

  push_x1_x10_x12

  popda x10         # Value to print
  li x11, CELLBITS  # Number of bits left

1:pushdatos
  srli x8, x10, CELLBITS-4
  andi x8, x8, 0xF
  li x12, 10

  bltu x8, x12, 2f
    addi x8, x8, 55-48
2:addi x8, x8, 48
  call emit

  slli x10, x10, 4
  addi x11, x11, -4
  bne x11, zero, 1b

  pop_x1_x10_x12
  j space


# -----------------------------------------------------------------------------
  Definition Flag_visible, ".rs"  # Print out return stack
# -----------------------------------------------------------------------------

  push_x1_x10_x11
  write "Returnstack: [ "

  # Calculate number of elements on datastack
  laf x15, gp_tp_sp
  lc x15, 8(x15)
  sub x15, x15, sp
  blt x15, zero, 2f # Stack underflow ? Do not print a zillion of locations.
  srli x11, x15, CELLSHIFT
  addi x11, x11, 1 # Show one more element

  pushda x11
  call hexdot
  write "] "

  beq x11, zero, 2f # Don't print elements for an empty stack

  laf x15, gp_tp_sp
  lc x10, 8(x15) # Start here !

1: # Fetch stack element directly
   pushdatos
   lc x8, 0(x10)
   call hexdot

   addi x10, x10, -CELL
   addi x11, x11, -1
   bne x11, zero, 1b

2: writeln " *>"

  pop_x1_x10_x11
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "words" # Print list of words with debug information
words:
# -----------------------------------------------------------------------------
  push x1

  writeln ""

  call dictionarystart

1:# Adress:
  write "Address: "
  dup
  call hexdot

  # Link
  write "Link: "
  dup
  lc x8, 0(x8)
  call hexdot

  # Flags
  write "Flags: "
  dup
  lc x8, CELL(x8)
  call hexdot

  # Entryaddress
  write "Code: "
  dup
  addi x8, x8, 2*CELL
  call skipstring
  call hexdot

  write "Name: "
  dup
  addi x8, x8, 2*CELL
  call ctype

  writeln ""

  call dictionarynext
  popda x15
  beq x15, zero, 1b

  drop

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "unused" # Bytes free for compilation in current memory area
# -----------------------------------------------------------------------------
  push x1
  call here

  call compiletoememq
  popda x15
  bne x15, zero, unused_emem

    call ramvarhere
    popda x15
    j unused_common

unused_emem:
  laf x15, __forth_emem_end

unused_common:
  sub x8, x15, x8

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "h.s"  # Print out data stack
# -----------------------------------------------------------------------------
  la x15, hexdot
  j 1f

# -----------------------------------------------------------------------------
  Definition Flag_visible, ".s"  # Print out data stack
# -----------------------------------------------------------------------------
  la x15, dot
  j 1f

# -----------------------------------------------------------------------------
  Definition Flag_visible, "u.s"  # Print out data stack
# -----------------------------------------------------------------------------
  la x15, udot

1:push_x1_x10_x13
  mv x10, x15
  write "Stack: [ "

  laf x14, base
  lc x13, 0(x14)
  li x15, 10
  sc x15, 0(x14)

  # Calculate number of elements on datastack
  laf x11, __datastack # load start
  sub x11, x11, x9 # subtract current stack pointer
  blt x11, zero, 2f # Stack underflow ? Do not print a zillion of locations.
  srli x11, x11, CELLSHIFT

  pushda x11
  call dot
  write "] "

  laf x14, base
  sc x13, 0(x14)

  blt x11, zero, 2f # Stack underflow ? Do not print a zillion of locations.

  beq x11, zero, 2f # Don't print elements for an empty stack

  laf x12, __datastack - CELL # Start here !

1: # Fetch stack element directly
   pushdatos
   lc x8, 0(x12)

    jalr x1, x10, 0

   addi x12, x12, -CELL
   addi x11, x11, -1
   bne x11, zero, 1b

2: # Print TOS
   write " TOS: "
   pushda x8

    jalr x1, x10, 0

   writeln " *>"

   pop_x1_x10_x13
   ret


  .ifdef debug # Only necessary for deep core debugging...

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hex.s"  # Print out data stack, do not use number subsystem
dots:
# -----------------------------------------------------------------------------

  push_x1_x10_x11
  write "Stack: [ "

  # Calculate number of elements on datastack
  laf x11, __datastack # Load start
  sub x11, x11, x9 # and subtract stack pointer.
  blt x11, zero, 2f # Stack underflow ? Do not print a zillion of locations.
  srli x11, x11, CELLSHIFT

  pushda x11
  call hexdot
  write "] "

  beq x11, zero, 2f # Don't print elements for an empty stack

  laf x10, __datastack - CELL # Start here !

1: # Fetch stack element directly
   pushdatos
   lc x8, 0(x10)
   call hexdot

   addi x10, x10, -CELL
   addi x11, x11, -1
   bne x11, zero, 1b

2: # Print TOS
   write " TOS: "
   dup
   call hexdot
   writeln " *>"

   pop_x1_x10_x11
   ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "dump"
dump: # ( addr len -- )
# -----------------------------------------------------------------------------
  push_x1_x10
  writeln ""

  popda x10        # Length
  add x10, x10, x8 # Stop address

1:dup
  call hexdot
  write ": "

  pushdatos
  lwu x8, 0(x8)
  call hexdot
  writeln ""
  addi x8, x8, 4

  bgeu x10, x8, 1b

  drop
  pop_x1_x10
  ret

  .endif

