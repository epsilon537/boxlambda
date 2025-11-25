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

# Interrupt handler for RISC-V

.macro initinterrupt Name:req, Asmname:req, Routine:req, Alignment

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "irq-\Name" # ( -- addr )
  CoreVariable irq_hook_\Name
#------------------------------------------------------------------------------
  pushdatos
  laf x8, irq_hook_\Name
  ret
  .varinit \Routine  # Start value for unused interrupts

  .ifb \Alignment
    .align 2
  .else
    .align \Alignment
  .endif

\Asmname:

  addi sp, sp, -13*CELL
  sc x1,  12*CELL(sp)
  laf x1, irq_hook_\Name
  j irq_common

.endm

#------------------------------------------------------------------------------
irq_common: # Common framework for all interrupt entries
#------------------------------------------------------------------------------

  sc x14, 11*CELL(sp) # Required for Forth core...
  sc x15, 10*CELL(sp)

  sc x16,  9*CELL(sp) # Required for Acrobatics only...
  sc x17,  8*CELL(sp)
  sc x18,  7*CELL(sp)
  sc x19,  6*CELL(sp)
  sc x20,  5*CELL(sp)
  sc x21,  4*CELL(sp)
  sc x22,  3*CELL(sp)
  sc x23,  2*CELL(sp)
  sc x24,  1*CELL(sp)
  sc x25,  0*CELL(sp)

  lc x1, 0(x1)
  jalr x1, x1, 0

  lc x25,  0*CELL(sp) # Required for Acrobatics only...
  lc x24,  1*CELL(sp)
  lc x23,  2*CELL(sp)
  lc x22,  3*CELL(sp)
  lc x21,  4*CELL(sp)
  lc x20,  5*CELL(sp)
  lc x19,  6*CELL(sp)
  lc x18,  7*CELL(sp)
  lc x17,  8*CELL(sp)
  lc x16,  9*CELL(sp)

  lc x15, 10*CELL(sp) # Required for Forth core...
  lc x14, 11*CELL(sp)
  lc x1,  12*CELL(sp)

  addi sp, sp, 13*CELL

  mret

