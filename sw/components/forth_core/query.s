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

# Input routine Query - with Unicode support.

#  # -----------------------------------------------------------------------------
#    Definition Flag_visible, "cexpect" # ( cstr-addr maxlength ) Collecting your keystrokes into a counted string !
#  # -----------------------------------------------------------------------------
#    push {lr}
#    ldr r0, [psp]  # Fetch address
#    push {r0}
#    adds r0, #1    # Add one to skip length byte for accept area
#    str r0, [psp]
#    bl accept
#    pop {r0}
#    strb tos, [r0] # Store accepted length into length byte of counted string
#    drop
#    pop {pc}

# -----------------------------------------------------------------------------
  Definition Flag_visible, "accept"
accept: # ( c-addr maxlength -- length ) Collecting your keystrokes !
# -----------------------------------------------------------------------------
  push_x1_x10_x12

  # Registers:
  # x10: The pressed key
  # x11: Buffer pointer
  # x12: Buffer fill gauge
  #  x8: Maximum length

  popdanos x11  # Fetch buffer address
  li x12, 0     # Currently zero characters typed

1: # Character collection loop
  call key
  popda x10

  li x15, 127        # Delete
  beq x10, x15, 5f   # Should do the same as backspace

  li x15, 32         # ASCII 0-31 are control characters which need special handling
  bltu x10, x15, 3f  # Branch if this is a control character.

   # Just add a normal character and echo it back. Do this only if there is buffer space left.
2: bgeu x12, x8, 1b # No more characters if buffer is full !

    pushda x10
    call emit

    sb x10, 0(x11)
    addi x11, x11, 1
    addi x12, x12, 1
    j 1b

3: # Handle control characters

  li x15, 9  # Replace TAB with space and include as normal character.
  bne x10, x15, 4f
    li x10, 32
    j 2b
4:

  li x15, 10    # Finish with LF
  beq x10, x15, 7f
  li x15, 13    # Finish with CR
  beq x10, x15, 7f

  li x15, 8          # Backspace ?
  bne x10, x15, 1b   # Ignore all other control characters.

5: # Delete a character.
  beq x12, zero, 1b  # Zero characters in buffer ? Then we cannot delete one.

  call dotquote # Clear a character visually. Emit sequence to delete one character in terminal.
  .byte 3, 8, 32, 8       # Step back cursor, overwrite with space, step back cursor again.

  # pushdaconst 8
  # call emit
  # pushdaconst 32
  # call emit
  # pushdaconst 8
  # call emit


      # Unicode-Zeichen sind so aufgebaut:
      # 11xx xxxx,  10xx xxxx,  10xx xxxx......
      # Wenn das letzte Zeichen also vorne ein 10 hat,
      # muss ich so lange weiterlöschen, bis ich eins mit 11 vorne erwische.
      # Prüfe natürlich immer, ob der Puffer vielleicht schon leer ist. Ausgetrickst !

      # Remove character from buffer and watch for Unicode !
      # Unicode: Maybe I have to remove more than one byte from buffer.
      # Unicode-Characters have this format:
      # 11xx xxxx,  10xx xxxx,  10xx xxxx......
      # When the last character has 10... then I have to delete until i reach a character that has 11....
      # Always check if buffer may be already empty !

6:beq x12, zero, 1b  # Anything available to be deleted ?

  addi x11, x11, -1
  addi x12, x12, -1
  lbu x10, 0(x11)    # Fetch cut off character from the end

  andi x15, x10, 0x80  # Check character for Unicode, is MSB set ?
  beq x15, zero, 1b    # If not, then this has been a normal character and my task is finished.

  andi x15, x10, 0x40  # Have I reached the first byte of this particular Unicode character yet ?
  bne x15, zero, 1b    # Properly deleted the whole Unicode character.
  j 6b                 # No ? Remove one more byte from the string.


7: # Return has been pressed: Store string length, print space and leave.
  mv x8, x12
  pop_x1_x10_x12
  j space

# -----------------------------------------------------------------------------
  Definition Flag_foldable_0, "tib" # ( -- addr )
# -----------------------------------------------------------------------------
tib:
  pushdaaddrf Inputbuffer
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, ">in" # ( -- addr )
  CoreVariable Bufferlevel
# -----------------------------------------------------------------------------
source_in:
  pushdaaddrf Bufferlevel
  ret
  .varinit 0

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_2variable, "current-source" # ( -- addr )
  DoubleCoreVariable current_source
#------------------------------------------------------------------------------
  pushdaaddrf current_source
  ret
  .varinit 0              # Empty TIB for default
  .varinit Inputbuffer


# -----------------------------------------------------------------------------
  Definition Flag_visible, "setsource" # ( c-addr len -- )
setsource:
# -----------------------------------------------------------------------------
  laf x15, current_source
  sc x8,    0(x15)
  drop
  sc x8, CELL(x15)
  drop

  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "source" # ( -- c-addr len )
source:
# -----------------------------------------------------------------------------
  push x10
  laf x10, current_source

  pushdatos
  lc x8, CELL(x10)
  pushdatos
  lc x8,    0(x10)

  pop x10
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "query" # Collecting your keystrokes into TIB ! Forth at your fingertips :-)
query:
# -----------------------------------------------------------------------------
  push x1

  call source_in # Aktueller Offset in den Inputbuffer  Zero characters consumed yet
  li x15, 0
  sc x15, 0(x8)
  drop

  call tib
  dup
  pushdaconst Maximuminput
  call accept
  pop x1
  j setsource

