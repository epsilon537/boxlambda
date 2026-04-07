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

# Terminal redirection hooks.

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-emit" # ( -- addr )
  CoreVariable hook_emit
#------------------------------------------------------------------------------
  pushdaaddrf hook_emit
  ret
  .varinit serial_emit  # Serial communication for default

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-key" # ( -- addr )
  CoreVariable hook_key
#------------------------------------------------------------------------------
  pushdaaddrf hook_key
  ret
  .varinit serial_key  # Serial communication for default

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-emit?" # ( -- addr )
  CoreVariable hook_qemit
#------------------------------------------------------------------------------
  pushdaaddrf hook_qemit
  ret
  .varinit serial_qemit  # Serial communication for default

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-key?" # ( -- addr )
  CoreVariable hook_qkey
#------------------------------------------------------------------------------
  pushdaaddrf hook_qkey
  ret
  .varinit serial_qkey  # Serial communication for default

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-pause" # ( -- addr )
  CoreVariable hook_pause
#------------------------------------------------------------------------------
  pushdaaddrf hook_pause
  ret
  .varinit nop_vector  # No Pause defined for default

#------------------------------------------------------------------------------
  Definition Flag_visible, "nop" # ( -- )
nop_vector:
#------------------------------------------------------------------------------
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "emit" # ( c -- )
emit:
#------------------------------------------------------------------------------
  laf x15, hook_emit
  lc x15, 0(x15)
  jalr zero, x15, 0

#------------------------------------------------------------------------------
  Definition Flag_visible, "key" # ( -- c )
key:
#------------------------------------------------------------------------------
  laf x15, hook_key
  lc x15, 0(x15)
  jalr zero, x15, 0

#------------------------------------------------------------------------------
  Definition Flag_visible, "emit?" # ( -- ? )
#------------------------------------------------------------------------------
  laf x15, hook_qemit
  lc x15, 0(x15)
  jalr zero, x15, 0

#------------------------------------------------------------------------------
  Definition Flag_visible, "key?" # ( -- ? )
#------------------------------------------------------------------------------
  laf x15, hook_qkey
  lc x15, 0(x15)
  jalr zero, x15, 0

#------------------------------------------------------------------------------
  Definition Flag_visible, "pause" # ( -- ? )
pause:
#------------------------------------------------------------------------------
  laf x15, hook_pause
  lc x15, 0(x15)
  jalr zero, x15, 0

.macro Debug_Terminal_Init

  # A special initialisation sequence intended for debugging
  # Necessary when you wish to use the terminal before running catchflashpointers.

  # -----------------------------------------------------------------------------
  # Initialize essential registers

  # Restore RS
  laf x15, gp_tp_sp
  lc sp, 8(x15)
  li  x8, 42          # TOS
  laf x9, __datastack # PSP
  # -----------------------------------------------------------------------------
  # x16-x31: Unused.
  # -----------------------------------------------------------------------------

   la x15, serial_emit
   laf x14, hook_emit
   sc x15, 0(x14)

   la x15, serial_qemit
   laf x14, hook_qemit
   sc x15, 0(x14)

   la x15, serial_key
   laf x14, hook_key
   sc x15, 0(x14)

   la x15, serial_qkey
   laf x14, hook_qkey
   sc x15, 0(x14)

   la x15, nop_vector
   laf x14, hook_pause
   sc x15, 0(x14)

.endm

