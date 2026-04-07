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

# Case structure

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "case"
  # ( -- 0 8 )
#------------------------------------------------------------------------------
  pushdaconst 0 # Current number of branches
  pushdaconst 8 # Structure pattern
  ret

# Small test:
# : wahl case 1 of ." Eins" endof 2 of ." Zwei" endof dup 3 = ?of ." Drei?" endof ." Andere" endcase ;

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "?of"
  # ( ... #of 8 -- ... addr #of+1 9 )
  # Takes flag instead of constant to build your own comparisions.
#------------------------------------------------------------------------------
  li x15, 8                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, structures_dont_match
  drop

  push x1
  to_r

  call structure_if

  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "of"
  # ( ... #of 8 -- ... addr #of+1 9 )
#------------------------------------------------------------------------------
  li x15, 8                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, structures_dont_match
  drop

  push x1
  to_r

  pushdaaddr of_opcodes
  call inlinekomma

    call here
    call four_allot

    pushdaconst 2           # Structure matching

1:call dropkomma

  r_from
  addi x8, x8, 1 # One more location a branch opcode has to be written in later.

  pushdaconst 9  # Structure pattern

  pop x1
  ret

of_opcodes:
  mv x15, x8
  drop
  sub x15, x15, x8
  sltiu x15, x15, 1

  # A bne opcode doesn't require a well-formed flag.
  # addi x15, x15, -1
  # inv x15

  ret  # End for inline,

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "endof"
  # ( ... addr-jne #of 9 -- ... addr-jmp #of 8 )
structendof:
#------------------------------------------------------------------------------
  li x15, 9                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, structures_dont_match
  drop

  push x1
  to_r # Move #of to Returnstack and out of the way

  call structure_else

  r_from # fetch back of#
  pushdaconst 8
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "endcase"
  # ( ... addrs-jmp #of 8 -- )
structendcase:
#------------------------------------------------------------------------------
  li x15, 8                  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, structures_dont_match
  drop

  push_x1_x10
  popda x10

  call dropkomma # Discard case value

1:beq x10, zero, 2f
  call structure_then
  addi x10, x10, -1
  j 1b

2:pop_x1_x10
  ret

