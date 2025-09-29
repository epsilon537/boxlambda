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

# Case-Struktur
# Case structure

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "case"
  # ( -- 0 8 )
#------------------------------------------------------------------------------
  pushdaconst 0 # Zahl der Zweige    Current number of branches
  pushdaconst 8 # Strukturerkennung  Structure pattern
  ret

# Small test:
# : wahl case 1 of ." Eins" endof 2 of ." Zwei" endof dup 3 = ?of ." Drei?" endof ." Andere" endcase ;

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "?of"
  # ( ... #of 8 -- ... addr #of+1 9 )
  # Nimmt einen Flag statt einer Konstanten entgegen.
  # Kann so eigene Vergleiche aufbauen.
  # Takes flag instead of constant to build your own comparisions.
#------------------------------------------------------------------------------
  li x15, 8                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, strukturen_passen_nicht
  drop

  push x1
  to_r

  call struktur_if

  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "of"
  # ( ... #of 8 -- ... addr #of+1 9 )
#------------------------------------------------------------------------------
  li x15, 8                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, strukturen_passen_nicht
  drop

  push x1
  to_r

  pushdaaddr of_opcodes
  call inlinekomma

    call here
    call four_allot

    .ifdef mipscore # NOP for branch delay slot
    pushdaconst 0
    call wkomma
   .endif

    pushdaconst 2           # Strukturerkennung  Structure matching

1:call dropkomma

  r_from
  addi x8, x8, 1 # Eine Adresse mehr, die abzuarbeiten ist  One more location a branch opcode has to be written in later.

  pushdaconst 9  # Strukturerkennung bereitlegen  Structure pattern

  pop x1
  ret

of_opcodes:
  mv x15, x8
  drop
  sub x15, x15, x8
  sltiu x15, x15, 1

  # Für einen bne zero Opcode braucht es kein wohlgeformtes Flag.
  # addi x15, x15, -1
  # inv x15

  ret  # Ende für inline,  End for inline,

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "endof"
  # ( ... addr-jne #of 9 -- ... addr-jmp #of 8 )
strukturendof:
#------------------------------------------------------------------------------
  li x15, 9                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, strukturen_passen_nicht
  drop

  push x1
  to_r # #of auf Returnstack  Move #of to Returnstack and out of the way

  call struktur_else

  r_from # #of zurückholen fetch back of#
  pushdaconst 8
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "endcase"
  # ( ... addrs-jmp #of 8 -- )
strukturendcase:
#------------------------------------------------------------------------------
  li x15, 8                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, strukturen_passen_nicht
  drop

  push_x1_x10
  popda x10

  call dropkomma # Discard case value

1:beq x10, zero, 2f
  call struktur_then
  addi x10, x10, -1
  j 1b

2:pop_x1_x10
  ret
