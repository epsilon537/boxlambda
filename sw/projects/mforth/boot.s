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


  # This is the same as in quit, in order to prepare for whatever the user might want to do within "init".

  laf sp, returnstackanfang
  laf x9, datenstackanfang

  .ifdef initflash
  call initflash
  .endif

  laf x14, base
  li x15, 10      # Base decimal
  sc x15, 0(x14)

  laf x14, state
  li x15, 0       # Execute mode
  sc x15, 0(x14)

  laf x14, konstantenfaltungszeiger
  # li x15, 0       # Clear constant folding pointer
  sc x15, 0(x14)

  laf x14, Pufferstand
  # li x15, 0       # Set >IN to 0
  sc x15, 0(x14)

  laf x14, current_source
  # li x15, 0       # Empty TIB is source
  sc x15, 0(x14)
  laf x15, Eingabepuffer
  sc x15, CELL(x14)


   # Suche nach der init-Definition:
   # Search for current init definition in dictionary:

   pushdaaddr init_name
   pushdaconst 4
   call find
   drop # No need for flags
   beq x8, zero, 1f

     # Gefunden ! Found !
     call execute
     j quit_intern
1:
   drop   # Die 0-Adresse von find. Wird hier heruntergeworfen, damit der Startwert AFFEBEEF erhalten bleibt !
   j quit # Drop 0-address of find to keep magic TOS value intact.

init_name: .byte 105, 110, 105, 116 # "init"
