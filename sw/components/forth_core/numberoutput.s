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
# Number output
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
  Definition Flag_visible, ".digit"
digitoutput: # ( u -- c ) Converts a digit into a character.
               # If base is bigger than 36, unprintable digits are written as #
# -----------------------------------------------------------------------------
  li x15, 10
  bgeu x8, x15, 1f # 0-9:
    addi x8, x8, 48 # Shift to beginning of ASCII numbers
    ret

1:li x15, 36
  bgeu x8, x15, 2f # A-Z:
    addi x8, x8, 55 # For small letters: 87.
    ret

2:li x8, 35 # Character #, if digit is not printable
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hold"
hold: # ( sign -- )  Insert one character at the beginning of number buffer
#------------------------------------------------------------------------------

  # Old String:  | Length     |     |
  # New String:  | Length + 1 | New |

  # Old String:  | Length     | I   | II  | III |     |
  # New String:  | Length + 1 | New | I   | II  | III |

  push x10

  laf x10, Numberbuffer
  lbu x14, 0(x10) # Get length

  li x15, Numberbufferlength  # Number buffer full ?
  bgeu x14, x15, 3f           # Do not accept any more characters.

  # Increment length
  addi x14, x14, 1
  sb x14, 0(x10)

  # Start moving with the end
  add x10, x10, x14 # Pointer to the free space for the new character

  li x15, 1 # Check if at least one character has to be moved
  beq x14, x15, 2f

1:# Move characters !
  addi x10, x10, -1
  lbu x15, 0(x10) # Fetch from current location-1
  sb x15, 1(x10)  # Write current location

  addi x14, x14, -1
  li x15, 1
  bne x14, x15, 1b # Until there is only one character left - the new one.

2:
  sb x8, 0(x10) # Insert new character

3:drop
  pop x10
  ret

#------------------------------------------------------------------------------
 Definition Flag_visible, "hold<"
appendnumber: # ( Character -- ) Insert one character at the end of number buffer
#------------------------------------------------------------------------------
  push x10

  laf x10, Numberbuffer
  lbu x14, 0(x10) # Get length

  li x15, Numberbufferlength  # Number buffer full ?
  bgeu x14, x15, 3f           # Do not accept any more characters.

    addi x14, x14, 1 # One more sign
    sb x14, 0(x10) # Write new length
    add x10, x10, x14
    sb x8, 0(x10) # Append new character at the end

3:drop
  pop x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "sign"
sign: # ( sign -- )
      # Checks flag of number on stack and adds a minus to number buffer if it is negative.
#------------------------------------------------------------------------------
  blt x8, zero, 1f
  drop
  ret

1:li x8, 45  # ASCII for minus
  j hold     # put it into number buffer

#------------------------------------------------------------------------------
  Definition Flag_visible, "#>" # ( Remaining digitsL (Remaining digitsH) -- Addr Len )
numberstringend: # Finishes a number string and gives back its address.
#------------------------------------------------------------------------------
  laf x15, Numberbuffer
  lbu x8, 0(x15)
  addi x15, x15, 1
  sc x15, 0(x9)
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "f#S"
trapdigits: # ( u -- u=0 )
      # Inserts all digits, at least one, into number buffer.
#------------------------------------------------------------------------------
  push_x1_x10

  li x10, CELLBITS

1:call fdigit
  addi x10, x10, -1
  bne x10, zero, 1b

  pop_x1_x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "f#"
fdigit: # ( u -- u )
      # Insert one more digit into number buffer
#------------------------------------------------------------------------------
  # Handles parts after decimal point
  # Idea: Multiply with base, next digit will be shifted into high-part of multiplication result.
  push x1
    pushdaaddrf base
    lc x8, 0(x8)
    call um_star     # ( After-Decimal-Point Base -- Low High )
    call digitoutput # ( Low=Still-after-decimal-point Character )
  pop x1
    j appendnumber # Add character to number buffer

#------------------------------------------------------------------------------
  Definition Flag_visible, "#S"
alldigits: # ( d-number -- d-number=0 )
      # Inserts all digits, at least one, into number buffer.
#------------------------------------------------------------------------------
  push x1

1:call onedigit
  bne x8, zero, 1b

  lc x15, 0(x9)
  bne x15, zero, 1b

  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "#"
onedigit: # ( number -- number )
      # Insert one more digit into number buffer
#------------------------------------------------------------------------------
  # Idea: Divide by base. Remainder is digit, Result is to be handled in next run.
  push x1
    pushdaaddrf base
    lc x8, 0(x8)   # Base-Low
    pushdaconst 0  # Base-High
    # ( uL uH BaseL BaseH )
    call ud_slash_mod
    # ( RemainderL RemainderH uL uH )
    call dswap
    # ( uL uH RemainderL RemainderH )
    drop
    # ( uL uH RemainderL )
    call digitoutput
    # ( uL uH digit )
  pop x1
    j hold
    # ( uL uH )

#------------------------------------------------------------------------------
  Definition Flag_visible, "<#" # ( d-Zahl -- d-Zahl )
digitstringentry: # Opens a number string
#------------------------------------------------------------------------------
  laf x14, Numberbuffer # Delete length, previous length zero.
  li x15, 0
  sb x15, 0(x14)
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "f."
      # ( Low High -- )
      # Prints a s31.32 number
#------------------------------------------------------------------------------
  pushdaconst CELLBITS
  j fdotn

#------------------------------------------------------------------------------
  Definition Flag_visible, "f.n"
      # ( Low High n -- )
      # Prints a s31.32 number with given number of fractional digits
fdotn:
#------------------------------------------------------------------------------
  push_x1_x10
  popda x10

  # ( Low High -- )
  call tuck # ( Sign Low High )
  call dabs # ( Sign uLow uHigh )

  pushdaconst 0  # ( Sign After-decimal-point=uL Before-decimal-point-low=uH Before-decimal-point-high=0 )
  call digitstringentry

  call alldigits # Processing of high-part finished. ( Sign uL 0 0 )
  drop # ( Sign uL 0 )

  li x8, 44 # Add a comma to number buffer ( Sign uL 44 )
  call appendnumber # ( Sign uL )

  beq x10, zero, 2f

1:call fdigit   # Processing of fractional parts ( Sign 0 )
  addi x10, x10, -1
  bne x10, zero, 1b

2:pop x10
  drop
  call sign

  pushdatos # Will be removed later
  pushdatos
  j terminate_numberoutput

#------------------------------------------------------------------------------
  Definition Flag_visible, "ud." # ( ud -- )
uddot:  # Prints an unsigned double number
#------------------------------------------------------------------------------
  # In Forth: <# #S #>
  push x1
  call digitstringentry
  call alldigits
  j terminate_numberoutput

#------------------------------------------------------------------------------
  Definition Flag_visible, "d." # ( d -- )
ddot:   # Prints a signed double number
#------------------------------------------------------------------------------
  push x1
  call tuck
  call dabs

  call digitstringentry
  call alldigits # ( Sign 0 0 )
  call rot
  call sign

terminate_numberoutput:
  call numberstringend
  call type
  pop x1
  j space

# -----------------------------------------------------------------------------
  Definition Flag_visible, "u."
      # ( Zahl -- )
      # Prints an unsigned single number
# -----------------------------------------------------------------------------
udot:
  pushdaconst 0 # Convert to unsigned double
  j uddot

# -----------------------------------------------------------------------------
  Definition Flag_visible, "." # ( number -- )
     # Prints a signed single number
# -----------------------------------------------------------------------------
dot:
  pushdatos
  srai x8, x8, SIGNSHIFT # s>d
  j ddot

