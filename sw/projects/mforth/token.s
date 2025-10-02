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

# Token und Parse zum Zerlegen des Inputbuffers
# Token and parse to cut contents of input buffer apart

# -----------------------------------------------------------------------------
  Definition Flag_visible, "token" # ( -- c-addr length )
token:
# -----------------------------------------------------------------------------
  pushdaconst 32
  j parse

# -----------------------------------------------------------------------------
  Definition Flag_visible, "parse" # ( c -- c-addr length )
parse:
# -----------------------------------------------------------------------------
  push_x1_x10_x12

  call source
  popdadouble x11, x10
  #popda x11  # Length  of input buffer
  #popda x10  # Pointer to input buffer

  call source_in # Current >IN gauge
  lc x12, 0(x8)
  drop

  add x14, x10, x12 # Start address of parsed string

  # Specifically for tokens, if the separator is a space:
  li x15, 32
  bne x8, x15, 2f

1:  beq x11, x12, 3f # Any characters left ?
      add x15, x10, x12
      lbu x15, 0(x15) # Fetch character
      bne x15, x8, 2f # Ist es das Leerzeichen ? Is this the delimiter which is space in this loop ?
        addi x12, x12, 1     # Don't collect spaces, advance >IN to skip.
        add x14, x10, x12   # Recalculate start address of parsed string
        j 1b

2:# Sammelschleife. Collecting loop.
  beq x11, x12, 3f # Any characters left ?

    add x15, x10, x12
    lbu x15, 0(x15)      # Fetch character
    addi x12, x12, 1     # Advance >IN
    bne x15, x8, 2b      # Is this the delimiter ?
      # Finished, fallthrough for delimiter detected.
      add x8, x10, x12
      sub x8, x8, x14
      addi x8, x8, -1    # Delimiter should not be part of the parsed string but needs to be count in >IN
      j 4f

3:# Finished. Fallthrough for end-of-string. Calculate length of parsed string
  add x8, x10, x12
  sub x8, x8, x14

4:# Store start address
  addi x9, x9, -CELL
  sc x14, 0(x9)

  # Store new >IN
  call source_in # Current >IN gauge
  sc x12, 0(x8)  # Fresh >IN gauge
  drop

  pop_x1_x10_x12
  ret
