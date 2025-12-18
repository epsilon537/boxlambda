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

.macro lowercase Register # Convert to lowercase.
  #    Hex Dec  Hex Dec
  # A  41  65   61  97  a
  # Z  5A  90   7A  122 z

  sltiu x14, \Register, 0x41
  bne x14, zero, 5f
    sltiu x14, \Register, 0x5B
    beq x14, zero, 5f
      addi \Register, \Register, 0x20
5:
.endm


# -----------------------------------------------------------------------------
  Definition Flag_visible, "compare"  # Compare two strings
compare:                              # ( c-addr1 len-1 c-addr2 len-2 -- f )
# -----------------------------------------------------------------------------

  push_x10_x12

  popdatriple x10, x11, x12
#  popda x10 # Length  of second string
#  popda x11 # Address of second string
#  popda x12 # Length  of first  string
#        x8  # Address of first string

  beq x10, x12, 2f
    # Lengths not equal.
1:  mv x8, zero
    j 4f

  # Lengths are equal. Compare characters.

2:beq x10, zero, 3f    # Any characters to compare left ?

  lbu x12, 0(x11)
  lowercase x12
  lbu x15, 0(x8)
  lowercase x15
  bne x12, x15, 1b

  addi x10, x10, -1
  addi x11, x11, 1
  addi x8,  x8,  1
  j 2b

3:li x8, -1

4:pop_x10_x12
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cr"
# -----------------------------------------------------------------------------
  push x1
  writeln ""
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bl"
# -----------------------------------------------------------------------------
  pushdaconst 32
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "space"
# -----------------------------------------------------------------------------
space:
  pushdaconst 32
  j emit

# -----------------------------------------------------------------------------
  Definition Flag_visible, "spaces"
# -----------------------------------------------------------------------------
  push x1

1:bge zero, x8, 2f
  call space
  addi x8, x8, -1
  j 1b

2:drop
  pop x1
  ret


#------------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[char]" # ( -- )  Get character from input stream and compile it as literal
#------------------------------------------------------------------------------
  j holechar

#------------------------------------------------------------------------------
  Definition Flag_visible, "char" # Get character from input stream
holechar: # ( -- character )
#------------------------------------------------------------------------------
  push x1
  call token      # Fetch token
  drop            # Drop length
  lbu x8, 0(x8)   # Read character
  pop x1
  ret

# -----------------------------------------------------------------------------
#  Definition Flag_immediate|Flag_foldable_0|Flag_Sprungschlucker, "(" # The comment
  Definition Flag_immediate|Flag_foldable_0, "(" # Der Kommentar
# -----------------------------------------------------------------------------
  pushdaconst 41 # Die Klammer )
  j 1f

# -----------------------------------------------------------------------------
#  Definition Flag_immediate|Flag_foldable_0|Flag_Sprungschlucker, "\\" # The long comment
  Definition Flag_immediate|Flag_foldable_0, "\\"
# -----------------------------------------------------------------------------
  pushdaconst 0 # Zero never occours - always catches whole line.

1:push x1
  call parse
  ddrop
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, ".\"" # Print a message
# -----------------------------------------------------------------------------
  pushdaaddr dotquote

1:push x1
  call callkomma

  pushdaconst 34 # Double quote "
  call parse
  pop x1
  j stringkomma

# -----------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "c\"" # Insert a string-literal
# -----------------------------------------------------------------------------
  pushdaaddr dotcquote
  j 1b

# -----------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "s\"" # Insert a string-literal
# -----------------------------------------------------------------------------
  pushdaaddr dotsquote
  j 1b

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(.\")"
dotquote:
# -----------------------------------------------------------------------------
  pushda x1

  # Skip the string in return address
  lbu x15, 0(x1)    # Length of string
  addi x1, x1, 1   # Skip length byte
  add x1, x1, x15   # Skip string

  # Make x1 4-even
  andi x15, x1, 1
  add  x1, x1, x15
  andi x15, x1, 2
  add  x1, x1, x15

  j ctype

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(c\")"
dotcquote:
# -----------------------------------------------------------------------------
  pushda x1

  # Skip the string in return address
  lbu x15, 0(x1)    # Length of string
  addi x1, x1, 1   # Skip length byte
  add x1, x1, x15   # Skip string

  # Make x1 4-even
  andi x15, x1, 1
  add  x1, x1, x15
  andi x15, x1, 2
  add  x1, x1, x15

  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(s\")"
dotsquote:
# -----------------------------------------------------------------------------
  pushda x1

  # Skip the string in return address
  lbu x15, 0(x1)    # Length of string
  addi x1, x1, 1   # Skip length byte
  add x1, x1, x15   # Skip string

  # Make x1 4-even
  andi x15, x1, 1
  add  x1, x1, x15
  andi x15, x1, 2
  add  x1, x1, x15

  j count

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(n\")"
dotnquote:
# -----------------------------------------------------------------------------
  push_x1_x10

  lbu x15, 0(x8)     # Fetch length
  beq x15, zero, 2f  # Empty string ?
  addi x8, x8, 1    # Skip length byte
  add x10, x8, x15   # End of string

1:lbu x15, 0(x8)

  srli x14, x15, 4
  add x14, x14, x10
  lbu x14, 0(x14)

  andi x15, x15, 0x0F
  add x15, x15, x10

  addi x9, x9, -2*CELL
  sc x8,  CELL(x9)
  sc x14,    0(x9)
  lbu x8,    0(x15)

  call emit
  call emit

  addi x8, x8, 1
  bne x8, x10, 1b

  j 2f

# -----------------------------------------------------------------------------
  Definition Flag_visible, "ctype"
ctype:
# -----------------------------------------------------------------------------
  push_x1_x10

  lbu x10, 0(x8)   # Length
  addi x8, x8, 1   # Skip length byte

  j type_intern

# -----------------------------------------------------------------------------
  Definition Flag_visible, "type"
type:
# -----------------------------------------------------------------------------
  push_x1_x10

  popda x10 # Length

type_intern:
  beq x10, zero, 2f

1:dup
  lbu x8, 0(x8)
  call emit

  addi x8, x8, 1
  addi x10, x10, -1
  bne x10, zero, 1b

2:drop
  pop_x1_x10
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "count"
count: # ( c-addr -- a-addr len ) Print a counted string
# -----------------------------------------------------------------------------
  # Count should shift the address one place and get the length.
  addi x8, x8, 1
  dup
  lbu x8, -1(x8)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "skipstring"
skipstring:
# -----------------------------------------------------------------------------
  # Skip the string in x8

  lbu x15, 0(x8)  # Length of string
  addi x8, x8, 1 # Skip length byte
  add x8, x8, x15 # Skip string

  # Make x8 4-even
  andi x15, x8, 1
  add  x8, x8, x15
  andi x15, x8, 2
  add  x8, x8, x15

  ret

