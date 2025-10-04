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
.section .forth_core

# -----------------------------------------------------------------------------
# Include the Forth core of Mecrisp-Quintus
# -----------------------------------------------------------------------------
.include "forth-core.s"

# -----------------------------------------------------------------------------
.global forth_init
forth_init: # Forth environment initialization.
# -----------------------------------------------------------------------------
  push ra  # We're called from C: save ra, gp and tp on the stack
  push gp
  push tp
  push x8 # Callee saved
  push x9 # Callee saved

  # Save the sp in a variable so we can restore it to this point if necessary.
  laf x15, returnstackstart
  sc sp, 0(x15)

  call Reset

  #Returning to C: retrieve x9, x8, tp, gp and ra from the stack
  pop x9
  pop x8
  pop tp
  pop gp
  pop ra

  ret

# -----------------------------------------------------------------------------
.global forth_repl
forth_repl: # Enter the Forth REPL
# -----------------------------------------------------------------------------
  push ra  # We're called from C: save ra, gp, tp, x8 and x9 on the stack
  push gp
  push tp
  push x8 # Callee saved
  push x9 # Callee saved

  # Save the sp in a variable so we can restore it to this point if necessary.
  laf x15, returnstackstart
  sc sp, 0(x15)
  j quit

# -----------------------------------------------------------------------------
Reset:
# -----------------------------------------------------------------------------

  #Restore RS in case we got called from reset word.
  laf x15, returnstackstart
  lc sp, 0(x15)

  push ra

  .include "catchmempointers.s"

  welcome " for RISC-V RV32IM by Matthias Koch." "BoxLambda port by Ruben Lysens/Epsilon537."

  # Ready to fly :!
  .include "boot.s"

  pop ra

  ret

# -----------------------------------------------------------------------------
.global forth_find
forth_find: # C interface for find
# -----------------------------------------------------------------------------
  push ra  # We're called from C: save ra, gp and tp on the stack
  push gp
  push tp
  push x8 # Callee saved
  push x9 # Callee saved

  mv x8, a0
  mv x9, a1

  call find

  mv a0, x8
  mv a1, x9

  pop x9
  pop x8
  pop tp
  pop gp
  pop ra

  ret

# -----------------------------------------------------------------------------
.global forth_execute
forth_execute: # C interface for execute
# -----------------------------------------------------------------------------
  push ra  # We're called from C: save ra, gp and tp on the stack
  push gp
  push tp
  push x8 # Callee saved
  push x9 # Callee saved

  mv x8, a0
  mv x9, a1

  call execute

  mv a0, x8
  mv a1, x9

  pop x9
  pop x8
  pop tp
  pop gp
  pop ra

  ret
