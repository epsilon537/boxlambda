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

# Universal Flash experimentation toolkit.
# It allows you to write your own Flash controller code directly in Forth.
# Just fill in these three hooks.
# Do compiletoram, set hooks, compiletoflash, set hooks in init.

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-initflash" # ( -- addr )
  CoreVariable hook_initflash
#------------------------------------------------------------------------------
  pushdaconst hook_initflash
  ret
  .varinit nop_vektor

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-flash!" # ( -- addr )
  CoreVariable hook_flashstore
#------------------------------------------------------------------------------
  pushdaconst hook_flashstore
  ret
  .varinit store

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-flushflash" # ( -- addr )
  CoreVariable hook_flushflash
#------------------------------------------------------------------------------
  pushdaconst hook_flushflash
  ret
  .varinit nop_vektor


#------------------------------------------------------------------------------
  Definition Flag_visible, "initflash" # ( -- )
initflash:
#------------------------------------------------------------------------------
  li x15, hook_initflash
  lc x15, 0(x15)
  .ifdef thejas32_pipeline_bug
  fence
  .endif
  jalr zero, x15, 0

# -----------------------------------------------------------------------------
  Definition Flag_visible, "flash!"
flashstore: # ( x addr -- )
# -----------------------------------------------------------------------------
  # Check if address is even

  andi x15, x8, 3
  beq x15, zero, 1f
    writeln "flash! needs 4-even addresses."
    j quit
1:

  # Check if address is outside of Forth core
  li x14, FlashDictionaryAnfang
  bltu x8, x14, 2f

  # Fine !
  li x15, hook_flashstore
  lc x15, 0(x15)
  .ifdef thejas32_pipeline_bug
  fence
  .endif
  jalr zero, x15, 0

2:writeln "Cannot write into core !"
  j quit

#------------------------------------------------------------------------------
  Definition Flag_visible, "flushflash" # ( -- )
flushflash:
#------------------------------------------------------------------------------
  li x15, hook_flushflash
  lc x15, 0(x15)
  .ifdef thejas32_pipeline_bug
  fence
  .endif
  jalr zero, x15, 0
