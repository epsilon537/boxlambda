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

# The C Foreign Function Interface

# -----------------------------------------------------------------------------
  Definition Flag_visible, "call-c"
# ( i*x c-fun -- j*x )
# -----------------------------------------------------------------------------
  push_x1_x3_x7_x10_x13

  popda x15 # Pop the C function pointer from the stack.

  # Set up the datastack object for C to use.
  laf x14, datastack
  sc x8, 0(x14)
  sc x9, 4(x14)

  #Set up gp and tp
  laf x3, gp_tp_sp
  lw x3, 0(x3)
  lw x4, 4(x3)

  # Save the original stack pointer and align the stack.
  mv x14, x2
  addi x2, x2, 16
  andi x2, x2, -16
  sc x14, -4(x2)

  # Call the C function
  jalr x1, x15

  # Restore the stack pointer
  lc x2, -4(x2)

  # Pick up the new datastack values. C might have modified it.
  laf x14, datastack
  lc x8, 0(x14)
  lc x9, 4(x14)

  pop_x1_x3_x7_x10_x13

  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bye"
# -----------------------------------------------------------------------------
  laf x15, gp_tp_sp

  # Set data stack
  mv a0, x8
  mv a1, x9

  # Restore stack, gp and tp to where it was at forth_repl entry (after the push_x1_x8_x9).
  laf x15, gp_tp_sp
  lc x3, 0(x15) # gp
  lc x4, 4(x15) # tp
  lc sp, 8(x15)

  pop_x1_x8_x9

  # The stack pointer was 16-byte aligned on entry. The stack is balanced at this
  # point, so the stack pointer will still be 16-byte aligned here.
  ret

