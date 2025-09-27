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

# Die zählenden Schleifen
# Counting loops

#------------------------------------------------------------------------------
  Definition Flag_inline, "k" # Kopiert den drittobersten Schleifenindex  Third loop index
#------------------------------------------------------------------------------
  # Returnstack ( Limit Index Limit Index )
  pushdatos
  lc x8, 2*CELL(sp)
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "j" # Kopiert den zweitobersten Schleifenindex  Second loop index
#------------------------------------------------------------------------------
  # Returnstack ( Limit Index )
  pushdatos
  lc x8, 0(sp)
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "i" # Kopiert den obersten Schleifenindex       Innermost loop index
#------------------------------------------------------------------------------
  # Returnstack ( )
  pushda x3
  ret


/* Ein paar Testfälle  Some tests

: table   cr 11 1 do
                    i 8 = if leave then
                    11 1 do
                           i j * . space
                           j 5 = if leave then
                           j 2 = if leave then
                         loop
                    cr
                  loop ;

: stars 0 ?do [char] * emit loop ;
: stars5 0 ?do [char] * emit   i 5 = if leave then loop ;

: table   cr 11 1 do 11 1 do i j * . space loop cr loop ;
: table   cr 11 1 do [ .s ] 11 1 do [ .s ] i j * . space loop [ .s ] cr loop [ .s ] ;
: table  cr 11 1 do i 8 = if leave then 11 1 do  i j * . space j 5 = if leave then  j 2 = if leave then loop cr loop ;

*/

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "leave" # Beendet die Schleife sofort.  Terminates loop immediately.
  # ( ... AlterLeavePointer 0 Sprungziel 3 ... )
  # --
  # ( ... AlterLeavePointer Vorwärtssprungziel{or-1-JZ} Sprungziel 3 ... )

  # ( ... OldLeavePointer 0 Target 3 ... )
  # --
  # ( ... OldLeavePointer Forward-Jump-Target{or-1-JZ} NumberofJumps Target 3 ... )
#------------------------------------------------------------------------------
  # Der LeavePointer zeigt an die Stelle, wo steht, wie viele Spezialsprünge noch abzuarbeiten sind.
  # Alle Stackelemente weiterschieben, Sprungadresse einfügen, Zähler erhöhen, Lücke anlegen.

  # LeavePointer points to the location which counts the number of jumps that have to be inserted later.
  # Leave moves all elements of stack further, inserts address for jump opcode, increments counter and allots space.

  push_x1_x10_x11

  # Agenda:
  # An dieser Stelle eine Vorwärtssprunglücke präparieren:
  # TOS bleibt TOS
  # Muss eine Lücke im Stack schaffen, alles NACH der Position des leavepointers muss weiterrücken.

  # Make a hole in datastack at the location the leavepointer points to for inserting the new location a jump opcode has to be patched in later
  # by moving all other elements further one place in datastack.

  laf x10, leavepointer
  lc x11, 0(x10) # Die Stelle, wohin er zeigt = Inhalt der Variable Leavepointer

  mv x14, x9 # Alter Stackpointer  Old stackpointer
  addi x9, x9, -CELL # Ein Element mehr auf dem Stack  One more element on stack after this

1:# Lückenschiebeschleife
  lc x15, 0(x14)
  sc x15, -CELL(x14)
  addi x14, x14, CELL
  bne x14, x11, 1b # x11 enthält die Stelle am Ende

# Zweimal schieben, weil für struktur-then zwei Elemente eingefügt werden sollen.

  mv x14, x9 # Alter Stackpointer  Old stackpointer
  addi x9, x9, -CELL # Ein Element mehr auf dem Stack  One more element on stack after this

1:# Lückenschiebeschleife
  lc x15, 0(x14)
  sc x15, -CELL(x14)
  addi x14, x14, CELL
  bne x14, x11, 1b # x11 enthält die Stelle am Ende


  # Muss jetzt die Stelle auf dem Stack, wo die Sprünge gezählt werden um Eins erhöhen
  # und an der freigewordenen Stelle die Lückenadresse einfügen.
  # Increment the number of jumps to be filled in later

  call here
  call four_allot

  .ifdef mipscore # NOP for branch delay slot
    pushdaconst 0
    call wkomma
  .endif

  popda x14 # Die Lückenadresse

  # Insert the address of location for jump opcode in datastack
  addi x11, x11, -CELL  # Weiter in Richtung Spitze des Stacks wandern
  sc x14, 0(x11) # Lückenadresse einfügen

  addi x11, x11, -CELL  # Weiter in Richtung Spitze des Stacks wandern
  li x15, 5 # Strukturerkennung für unbedingten Sprung
  sc x15, 0(x11) # Lückenadresse einfügen

  # Den neuen Leavepointer vermerken  Update leavepointer
  sc x11, 0(x10)

  # Increment counter for number of jumps to be generated later

  lc x15, -CELL(x11)  # Sprungzähler aus dem Stack kopieren
  addi x15, x15, 1    # Den Sprungzähler um eins erhöhen
  sc x15, -CELL(x11)  # und zurückschreiben.

  pop_x1_x10_x11
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "(do)"
#------------------------------------------------------------------------------
  pushdouble x4, x3
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "unloop" # Wirft die Schleifenstruktur vom Returnstack
unloop:                           # Remove loop structure from returnstack
#------------------------------------------------------------------------------
  popdouble x3, x4 # Hole die alten Schleifenwerte zurück  Fetch back old loop values
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "+loop" # Es wird benutzt mit ( Increment -- ).
  # ( AlterLeavePointer ... ZahlderAdressen Sprungziel 3 -- )

  # Usage: ( Increment -- ).
  # ( OldLeavePointer ... NumberofJumps Target 3 -- )
#------------------------------------------------------------------------------
  li x15, 3  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, strukturen_passen_nicht
  drop

  push_x1_x10

  pushdaaddr plusloop_opcodes
  call callkomma

  .ifdef mipscore
    li x10, 0x10000000 | reg_tmp1 << 21
  .else
    li x10, 0x00000063 | reg_tmp1 << 15 # beq x15, zero, 0
  .endif

  j loop_intern

#------------------------------------------------------------------------------

plusloop_opcodes:

  blt x8, zero, 1f

  # Positive increment:
  sub x14, x3, x4 # Index-Limit

    add x3, x3, x8
    drop

  sub x15, x3, x4 # Index+Inc-Limit

  sltu x15, x15, x14
  ret

1:# Negative increment:
  sub x14, x3, x4 # Index-Limit

    add x3, x3, x8
    drop

  sub x15, x3, x4 # Index+Inc-Limit

  sltu x15, x14, x15
  ret


#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "loop" # Es wird benutzt mit ( -- ).
  # ( AlterLeavePointer ... ZahlderAdressen Sprungziel 3 -- )

  # Usage: ( -- ).
  # ( OldLeavePointer ... NumberofJumps Target 3 -- )
#------------------------------------------------------------------------------
  li x15, 3  # Check for structure pattern: Give error message and quit if wrong.
  bne x8, x15, strukturen_passen_nicht
  drop

  push_x1_x10

  .ifdef mipscore
  pushdaconst 0x24000001 | reg_loop_index << 21 | reg_loop_index << 16
  .else
  pushdaconst 0x00100013 | reg_loop_index <<  7 | reg_loop_index << 15 # addi x3, x3, 1 # Increment index.
  .endif

  call wkomma

  .ifdef mipscore
    li x10, 0x14000000 | reg_loop_index << 21 | reg_loop_limit << 16
  .else
    li x10, 0x00001063 | reg_loop_index << 15 | reg_loop_limit << 20 # bne x3, x4, 0
  .endif

loop_intern:

  call here
  call minus

  .ifdef mipscore
    call generate_16bit_jump_encoding
    or x8, x8, x10
  .else
    call generate_sb_encoding
    or x8, x8, x10
  .endif

  call wkomma

  .ifdef mipscore # NOP for branch delay slot
    pushdaconst 0
    call wkomma
  .endif

  popda x10
1:beq x10, zero, 2f
  call struktur_then
  addi x10, x10, -1
  j 1b

2:pushdaaddr unloop
  call inlinekomma

  laf x14, leavepointer  # Zurückholen für die nächste Schleifenebene
  sc x8, 0(x14)         # Fetch back old leavepointer for next loop layer.
  drop

  pop_x1_x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "do"
  # Es wird benutzt mit ( Limit Index -- ).
  # ( -- AlterLeavePointer 0 Sprungziel 3 )

  # Usage: ( Limit Index -- ).
  # ( -- OldLeavePointer 0 Target 3 )
#------------------------------------------------------------------------------

  # LeavePointer points to the location which counts the number of jumps that have to be inserted later.
  # Leave moves all elements of stack further, inserts address for jump opcode, increments counter and allots space.

  push x1

  call inline_do_opcodes

  laf x14, leavepointer

  pushdatos
  lc x8, 0(x14) # Alten Leavepointer sichern  Save old leavepointer

  pushdaconst 0
  sc x9, 0(x14) # Aktuelle Position im Stack sichern  Save current position on datastack

do_intern_finish:
  call here               # Schleifen-Rücksprung vorbereiten  Prepare loop jump back to the beginning
  pushdaconst 3           # Strukturerkennung  Structure matching
  pop x1
  ret


inline_do_opcodes:
  pushdaaddr do_opcodes
  j inlinekomma

do_opcodes:
  pushdouble x4, x3
  popdadouble x3, x4
  ret  # Ende für inline,  End for inline,

#------------------------------------------------------------------------------
  Definition Flag_immediate_compileonly, "?do"
  # Es wird benutzt mit ( Limit Index -- ).
  # ( -- AlterLeavePointer Vorsprungadresse 1 Sprungziel 3 )
  # Diese Schleife springt sofort ans Ende, wenn Limit=Index.

  # Usage: ( Limit Index -- ).
  # ( -- OldLeavePointer Forward-Jump-Target 1 Target 3 )
  # This loop terminates immediately if Limit=Index.
#------------------------------------------------------------------------------
  push x1

  call inline_do_opcodes

  .ifdef mipscore
  pushdaconst 0x00000023 | reg_tmp1 << 11 | reg_loop_index << 21 | reg_loop_limit << 16
  .else
  pushdaconst 0x40000033 | reg_tmp1 <<  7 | reg_loop_index << 15 | reg_loop_limit << 20 # sub x15, x3, x4
  .endif
  call wkomma

  laf x14, leavepointer
  pushdatos
  lc x8, 0(x14) # Alten Leavepointer sichern  Save old leavepointer

    call here
    call four_allot

    .ifdef mipscore # NOP for branch delay slot
    pushdaconst 0
    call wkomma
   .endif

    pushdaconst 2           # Strukturerkennung  Structure matching

  pushdaconst 1

  laf x14, leavepointer
  sc x9, 0(x14) # Aktuelle Position im Stack sichern  Save current position on datastack

  j do_intern_finish
