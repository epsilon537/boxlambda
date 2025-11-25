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
# Swiches for capabilities of this chip
# -----------------------------------------------------------------------------

.option norelax
# The Forth core has its own section in the link map.
.section .forth_core

# -----------------------------------------------------------------------------
# Include the Forth core of Mecrisp-Quintus
# -----------------------------------------------------------------------------
.global EvaluateState

# forth-core.s is where the mecrisp forth core is instantiated. This happens
# through macro processing at the assembler preprocessing stage.
.include "forth-core.s"

# At this point, the core-dictionary has been generated.

# -----------------------------------------------------------------------------
.global forth_core_init_
forth_core_init_: # Forth core initialization. Called once, at boot time.
# -----------------------------------------------------------------------------
  # We're called from C: save ra, x8 and x9 on the stack
  # Since we're called from C, the stack pointer will be 16-byte aligned, following
  # the RISC-V C calling convention.
  push_x1_x8_x9

  # Save x3 and x4 (a.k.a. gp and tp) in gp_tp_sp global
  # so these registers can be set up properly when Forth calls C.
  # Save the sp in a variable so we can restore it to this point if
  # necessary (e.g. when reset word is invoked).
  laf x15, gp_tp_sp
  sc x3, 0(x15)
  sc x4, 4(x15)
  sc sp, 8(x15)

  # TOS and PSP (and other things) will be set up in the Reset function (in the catchmempointers code).

  call Reset

  # Set up the datastack object for C to use.
  laf x14, datastack
  sc x8, 0(x14)
  sc x9, 4(x14)

  # Returning to C: restore x9, x8, tp, gp and ra from the stack
  laf x15, gp_tp_sp
  lc x3, 0(x15) # gp
  lc x4, 4(x15) # tp

  # The stack pointer was 16-byte aligned on entry. The stack is balanced at this
  # point, so the stack pointer will still be 16-byte aligned.
  pop_x1_x8_x9
  ret

# -----------------------------------------------------------------------------
.global forth_repl_
forth_repl_: # Enter the Forth REPL
# -----------------------------------------------------------------------------
  # We're called from C: save ra, x8 and x9 on the stack
  # Since we're called from C, the stack pointer will be 16-byte aligned, following
  # the RISC-V C calling convention.
  push_x1_x8_x9

  # Get TOS and PSP from the datastack object.
  laf x14, datastack
  lc x8, 0(x14)
  lc x9, 4(x14)

  # We don't return from quit. If we want to exit from the REPL, the "bye" Word is
  # invoked which handles the return to C.
  j quit

# -----------------------------------------------------------------------------
Reset:
# -----------------------------------------------------------------------------

  #Restore RS in case we got called from reset word.
  laf x15, gp_tp_sp
  lc sp, 8(x15)

  push x1

  .include "catchmempointers.s"

  welcome " for RISC-V RV32IM by Matthias Koch." "BoxLambda port by Ruben Lysens/Epsilon537."

  # Ready to fly :!
  .include "boot.s"

  pop x1

  ret

# -----------------------------------------------------------------------------
.global forth_core_fun_
forth_core_fun_: # C interface to call a core forth function such as find or execute
# -----------------------------------------------------------------------------
  # We're called from C: save ra, x8 and x9 on the stack
  # Since we're called from C, the stack pointer will be 16-byte aligned, following
  # the RISC-V C calling convention.

  push_x1_x8_x9

  # Get TOS and PSP from the datastack object.
  laf x14, datastack
  lc x8, 0(x14)
  lc x9, 4(x14)

  popda x14

  jalr x14

  # Update the datastack object for C to use.
  laf x14, datastack
  sc x8, 0(x14)
  sc x9, 4(x14)

  # Restore impacted risc-v calling convention registers that were saved
  # at forth_reply entry
  laf x15, gp_tp_sp
  lc x3, 0(x15) # gp
  lc x4, 4(x15) # tp

  pop_x1_x8_x9

  # The stack pointer was 16-byte aligned on entry. The stack is balanced at this
  # point, so the stack pointer will still be 16-byte aligned.
  ret

