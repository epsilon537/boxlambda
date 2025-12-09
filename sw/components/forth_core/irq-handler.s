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

.globl \Asmname
\Asmname:

  laf x1, irq_hook_\Name
  j irq_common

.endm

#------------------------------------------------------------------------------
irq_common: # Common framework for all interrupt entries
#------------------------------------------------------------------------------
  # On BoxLambda we don't need to save/restore registers on the stack. BoxLambda
  # has a register bank for IRQ mode.

  lc x1, 0(x1)
  jalr x1, x1, 0

  mret

