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

# The compiler - parts that are the same for Flash and for Ram.
# This is mecrisp quintus inheritance. BoxLambda does not compile to flash.

# -----------------------------------------------------------------------------
  Definition Flag_visible, "registerliteral," # ( x Register -- )
registerliteralkomma: # Compile code to put a literal constant into a register.
# -----------------------------------------------------------------------------

  beq x8, zero, ddrop_vector # Don't load the zero register

  push_x1_x10
  popda x10 # Target register

  # Check if constant can be loaded more quickle:

  li x15, -2048 # 0xFFFFF800
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, 2f

1:# Short version with one opcode:

  sll x8, x8, 20
  li x15, 0x4013 # xori x0, x0, ...
  sll x14, x10, 7  # target register
  or  x15, x15, x14  # or-ing
  or x8, x8, x15
  call wkomma
  j 4f

2:# Long version with two opcodes:

  # Correction for negative sign:
  li x15, 0x800
  and x15, x15, x8
  beq x15, zero, 3f
    li x15, -4096 # 0xFFFFF000
    xor x8, x8, x15
3:

6:dup
  li x15, -4096 # 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x000000037  # lui x0, ...
  slli x14, x10, 7  # target register
  or  x8, x8, x14  #  or-ing
  call wkomma

7:
  slli x8, x8, 20
  bne x8, zero, 5f
    drop # If the xori constant is 0, no need to write a xori opcode.
    j 4f

5:li x15, 0x4013 # xori x0, x0, ...
  slli x14, x10, 7  # target register
  or  x15, x15, x14  #  or-ing
  slli x14, x10, 15 # source register
  or  x15, x15, x14  #  or-ing
  or x8, x8, x15
  call wkomma

4:pop_x1_x10
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "literal," # ( x -- )
literalkomma:
# -----------------------------------------------------------------------------
  push x1
  call dup_komma

  pushdaconst reg_tos
  pop x1
  j registerliteralkomma

dup_komma:
  pushdaaddr dup_jump
  j inlinekomma

# -----------------------------------------------------------------------------
  Definition Flag_visible, "call," # ( x -- )
callkomma: # Here we could also check if we can generate a short JAL version.
# -----------------------------------------------------------------------------

  push x1

  dup
  call here
  call minus

  call uj_encoding_q
  popda x15
  beq x15, zero, 2f

    # Short JAL-Opcode is possible.
    nip
    li x15, 0x000000ef # jal x1, 0
    or x8, x8, x15
    pop x1
    j wkomma

2:drop

  # Correction for negative sign:
  li x15, 0x800
  and x15, x15, x8
  beq x15, zero, 1f
    li x15, 0x00001000
    add x8, x8, x15
1:

  dup
  li x15, -4096 # 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x00000037 | reg_tmp1 << 7  # lui x15, ...
  call wkomma

  sll x8, x8, 20
  li x15, 0x000000e7 | reg_tmp1 << 15 # jalr x1, x15, 0
  or x8, x8, x15
  pop x1
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_visible, "inline," # ( x -- )
inlinekomma:
# -----------------------------------------------------------------------------

  push x1

1:# -------------------------------------

  lwu x14, 0(x8)
  li x15, 0xFFC10113  # addi x2, x2, -4
  bne x14, x15, 2f

    lwu x14, 4(x8)
    li x15, 0x00112023  # sw x1, 0(x2)
    bne x14, x15, 2f

      addi x8, x8, 8  # Do not inline these if they appear in a definition

2:# -------------------------------------

  lwu x14, 0(x8)
  li x15, 0x00012083  # lw x1, 0(x2)
  bne x14, x15, 2f

    lwu x14, 4(x8)
    li x15, 0x00410113  # addi x2, x2, 4
    bne x14, x15, 2f

      lwu x14, 8(x8)
      li x15, 0x00008067  # jalr   zero, 0 (x1) Ret-Opcode
      beq x14, x15, 3f

2:# -------------------------------------

  lwu x14, 0(x8)
  li x15, 0x00008067 # Ret-Opcode
  beq x14, x15, 3f

    pushda x14
    call wkomma
    addi x8, x8, 4
    j 1b

3:drop
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "skipdefinition" # ( addr -- addr )
findendofdefinition:
# -----------------------------------------------------------------------------
  li x14, 0x00008067 # Ret-Opcode

1:lwu x15, 0(x8)
  addi x8, x8, 4
  bne x15, x14, 1b

  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "string," # ( c-addr length -- )
stringkomma: # Write a string in Dictionary.
# -----------------------------------------------------------------------------
  push_x1_x10_x13

  andi x10, x8, 0xFF   # Maximum counted string length
  lc x11, 0(x9)        # Fetch address of string
  addi x9, x9, CELL
  call clearbytes
  call addbyte # Strings begins with its length byte

1:beq x10, zero, 2f
  pushdatos
  lbu x8, 0(x11)
  call addbyte
  addi x10, x10, -1
  addi x11, x11,  1
  j 1b

2:call flushbytes

  pop_x1_x10_x13
  ret

addbyte:
  sll x8, x8, x12
  or x13, x13, x8
  drop
  addi x12, x12, 8
  li x15, 32
  bne x12, x15, retbytes
flushbytes:
  beq x12, zero, clearbytes
  push x1
  pushda x13
  call wkomma

  pop x1
clearbytes:
  li x12, 0 # How many bytes already written ?
  li x13, 0 # Data which needs to be flushed later
retbytes:
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[']" # Searches the next token in input buffer and compiles its entry point as literal.
#------------------------------------------------------------------------------
  j tick # What it used to be: ['] ' [immediate] [0-foldable] ;

# -----------------------------------------------------------------------------
  Definition Flag_visible, "'" # Searches next token in unput buffer and gives back its code entry point.
tick:
# -----------------------------------------------------------------------------
  push_x1_x10_x11
  call token

    lc x10, 0(x9) # Save string address and length for later use
    mv x11, x8

  call find
  popda x15 # Drop Flags into x15 - used by postpone !

  bne x8, zero, 1f # Probe entry address

    pushdatos
    sc x10, 0(x9)
    mv x8, x11
    j type_not_found_quit

1:pop_x1_x10_x11
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "postpone" # Search next token and fill it in Dictionary in a special way.
#------------------------------------------------------------------------------
  push x1
  call tick # Stores Flags into x15 !
  # ( jumpadresse )

1:andi x14, x15, Flag_immediate & ~Flag_visible # In case definition is immediate: Compile a call to its address.
  bne x14, zero, 4f

2:andi x14, x15, Flag_inline & ~Flag_visible    # In case definition is inline: Compile entry point as literal and a call to inline, afterwards.
  beq x14, zero, 3f
                             # ( Entry-Address )
    call literalkomma                  # Compile Entry-Address as a constant.
    pushdaaddr inlinekomma
    j 4f                             # Ready to call.

3:# Normal                     # In case definition is normal: Compile entry point as literal and a call to call, afterwards.
    call literalkomma
    pushdaaddr callkomma

4:  pop x1
    j callkomma

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "exit"
exitkomma:  # Writes a ret opcode into current definition. Take care with inlining !
#------------------------------------------------------------------------------

  # Write opcodes for "pop x1 ret"

  push x1
  pushdaconst 0x00012083 # lw   x1, 0(x2)
  call wkomma
  pushdaconst 0x00410113 # addi x2, x2, 4
  call wkomma
  pop x1
  j retkomma

#------------------------------------------------------------------------------
  Definition Flag_visible, "ret," # ( -- )
retkomma:
#------------------------------------------------------------------------------

  pushdaconst 0x00008067 # ret
  j wkomma

# Some tests:
#  : fac ( n -- n! )   1 swap  1 max  1+ 2 ?do i * loop ;
#  : fac-rec ( acc n -- n! ) dup dup 1 = swap 0 = or if drop else dup 1 - rot rot * swap recurse then ; : facre ( n -- n! ) 1 swap fac-rec ;

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "recurse" # Execute freshly defined definition.
#------------------------------------------------------------------------------
  pushdaaddrf Entrypoint
  lc x8, 0(x8)
  j callkomma

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "state" # ( -- addr )
  CoreVariable state
# -----------------------------------------------------------------------------
  pushdatos
  laf x8, state
  ret
  .varinit 0

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "(sp)" # ( -- addr )
  CoreVariable DataStackBackup
# -----------------------------------------------------------------------------
  pushdatos
  laf x8, DataStackBackup
  ret
  .varinit 0

#------------------------------------------------------------------------------
  Definition Flag_visible, "]" # Switch to compile mode
compilemode:
# -----------------------------------------------------------------------------
  li x15, -1
  j 1f

#------------------------------------------------------------------------------
  Definition Flag_immediate, "[" # Switch to execute mode
executemode:
# -----------------------------------------------------------------------------
  li x15, 0
1:laf x14, state
  sc x15, 0(x14)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, ":" # ( -- )
# -----------------------------------------------------------------------------
  push x1

  laf x14, DataStackBackup
  sc x9, 0(x14)                # Save current datastack pointer to detect structure mismatch later.

  call create
  # call push_x1_komma

  pop x1
  # j compilemode
  li x15, 1
  j 1b

push_x1_komma:

      push x1

      pushdaconst 0xffc10113 # addi x2, x2, -4
      call wkomma
      pushdaconst 0x00112023 # sw x1, 0(x2)
      call wkomma

      pop x1
      ret

# -----------------------------------------------------------------------------
  Definition Flag_immediate_compileonly|Flag_noframe, ";" # ( -- )
# -----------------------------------------------------------------------------
  push x1

  laf x14, DataStackBackup
  lc x15, 0(x14)               # Check fill level of datastack.
  beq x15, x9, 1f
    writeln " Stack not balanced."
    j quit
1: # Stack balanced, ok

  # Check if writing a push x1 / pop x1 frame is necessary.

  laf x14, state
  lc x14, 0(x14)
  li x15, 1
  bne x14, x15, 2f
    call retkomma
    j 3f
2:  call exitkomma

3:call smudge
  pop x1
  j executemode

# -----------------------------------------------------------------------------
  Definition Flag_visible, "execute"
.global execute
execute:
# -----------------------------------------------------------------------------
  popda x15
  jalr zero, x15, 0

# -----------------------------------------------------------------------------
  Definition Flag_immediate, "[immediate]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_immediate & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[inline]" # ( -- )
setze_inlineflag:
# -----------------------------------------------------------------------------
  li x15, Flag_inline & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[noframe]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_noframe & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate, "[compileonly]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_immediate_compileonly & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[0-foldable]" # ( -- )
set_foldableflag:
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_0 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[1-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_1 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[2-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_2 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[3-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_3 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[4-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_4 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[5-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_5 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[6-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_6 & ~Flag_visible
  j setflags_x15

# -----------------------------------------------------------------------------
  Definition Flag_immediate|Flag_foldable_0, "[7-foldable]" # ( -- )
# -----------------------------------------------------------------------------
  li x15, Flag_foldable_7 & ~Flag_visible
setflags_x15:
  pushda x15
  j setflags

# -----------------------------------------------------------------------------
  Definition Flag_visible, "constant" # ( n -- )
# -----------------------------------------------------------------------------
  push x1
  call create
1:call literalkomma
  call retkomma

  call set_foldableflag
  pop x1
  j smudge

# -----------------------------------------------------------------------------
  Definition Flag_visible, "2constant" # ( n -- )
# -----------------------------------------------------------------------------
  push x1
  call create
  swap
  call literalkomma
  j 1b

