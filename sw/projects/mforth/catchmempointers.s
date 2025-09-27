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

# Initialisiert die Pointer und Flash-Variablen nach dem Neustart.
# Wird direkt eingefügt und nur einmal beim Start benutzt,
# deshalb werden Register hier nicht gesichert.

# Initialises pointers and variables for flash dictionary after Reset.
# This runs one time after Reset, no registers are saved here.


   # Hardware sets return stack pointer on startup from vector table.
   # Set Return stack pointer here (again) just in case this might be a software re-entry.

   # Return stack pointer already set up. Time to set data stack pointer !
   # Normaler Stackpointer bereits gesetzt. Setze den Datenstackpointer:

   # TOS setzen, um Pufferunterläufe gut erkennen zu können
   # TOS magic number to see spurious stack underflows in .s

  # -----------------------------------------------------------------------------
  # Initialize register file
  li  x1, 0           # Return address register, holds link back and is also used to compose long calls with auipc and jalr
  laf x2, returnstackanfang # Set return stack pointer (sp)
  li  x3, 0           # Loop index
  li  x4, 0           # Loop limit
  li  x5, 0           # Scratch register, needs to be saved.
  li  x6, 0           # Scratch register, needs to be saved.
  li  x7, 0           # Scratch register, needs to be saved.

  li  x8, 42          # TOS
  laf x9, datenstackanfang  # PSP Set data stack pointer
  li x10, 0           # Scratch register, needs to be saved.
  li x11, 0           # Scratch register, needs to be saved.
  li x12, 0           # Scratch register, needs to be saved.
  li x13, 0           # Scratch register, needs to be saved.
  li x14, 0           # Free scratch register, not saved across calls.
  li x15, 0           # Free scratch register, not saved across calls.
  # -----------------------------------------------------------------------------
# li x16, 0           # x16-x31: Unused.
# li x17, 0
# li x18, 0
# li x19, 0
# li x20, 0
# li x21, 0
# li x22, 0
# li x23, 0
# li x24, 0
# li x25, 0
# li x26, 0
# li x27, 0
# li x28, 0
# li x29, 0
# li x30, 0
# li x31, 0
  # -----------------------------------------------------------------------------

  # Suche nun im Flash nach Anfang und Ende.
  # Short: Search for begin and end in Flash.

   # Dictionarypointer ins RAM setzen
   # Set dictionary pointer into RAM first
   laf x14, Dictionarypointer
   laf x15, RamDictionaryAnfang
   sc x15, 0(x14)

   # Fadenende fürs RAM vorbereiten
   # Set latest for RAM
   laf x14, Fadenende
   la x15, CoreDictionaryAnfang
   sc x15, 0(x14)

 #  write "Setze Fadenende:"
 #  pushdaconst Fadenende
 #  lc x8, 0(x8)
 #  # laf x8, CoreDictionaryAnfang
 #  call hexdot
 #  writeln ""

  # Registerbelegung:  Register allocation here:

  #    x10 Für dies und das    Temporary this and that
  #    x11 Aktuelle Flags      Current Flags
  #    x13 Für dies und das    Temporary this and that
  #    x7  Belegtes Ram.       Keeps track of allocated RAM
  # TOS=x8 Adresshangelzeiger  Pointer that crawls through dictionary


  pushdatos
  la x8,  CoreDictionaryAnfang  # Hier fängt es an.  Start at the beginning
  laf x7, RamDictionaryEnde     # Fürs Abzählen des Variablenplatzes  Variables start at the end of RAM dictionary

SucheFlashPointer_Hangelschleife:
#  pushda x8
#  call hexdot
#  writeln "Hangelschleife"
  lc x11, CELL(x8)  # Aktuelle Flags lesen  Fetch current Flags

  li x13, Flag_invisible # Flag_invisible ? Überspringen !  Skip invisible definitions
  beq x11, x13, Sucheflashpointer_Speicherbelegung_fertig

  .ifdef erasedflashspecial
  li x13, erasedword # Leeren Flash überspringen !  Skip empty flash locations
  beq x11, x13, Sucheflashpointer_Speicherbelegung_fertig
  .endif

    # Dies Wort ist sichtbar. Prüfe, ob es Ram-Speicher anfordert und belegt.
    # This definition is visible. Check if it allocates RAM.

    andi x13, x11, Flag_buffer & ~Flag_visible
    beq x13, zero, 1f # No buffer requested.

      # Search for end of code of current definition.
      push x8

      addi x8, x8, 2*CELL
      call skipstring  # x8 zeigt nun an den Codebeginn des aktuellen Wortes.  x8 points to start of code of current definition
      call suchedefinitionsende # Advance pointer to end of code. This is detected by "bx lr" or "pop {pc}" opcodes.

      # x8 ist nun an der Stelle, wo die Initialisierungsdaten liegen. x8 now points to the location of the initialisation at the end of code of current definition.

      .ifdef compressed_isa
      lhu x13, 0(x8) # Fetch required length of buffer, do this in two steps because of alignment issues

      lhu x10, 2(x8)
      slli x10, x10, 16
      or x13, x13, x10

      .ifdef RV64
        lhu x10, 4(x8)
        slli x10, x10, 32
        or x13, x13, x10
        lhu x10, 6(x8)
        slli x10, x10, 48
        or x13, x13, x10
      .endif

      .else

      .ifdef RV64
        lwu x13, 0(x8)

        lwu x10, 4(x8)
        slli x10, x10, 32
        or x13, x13, x10
      .else
        lc x13, 0(x8) # Fetch required length of buffer
      .endif

      .endif

 #     pushda x13
 #     call hexdot
 #     writeln " buffer:"

      # Ramvariablenpointer wandern lassen  Subtract from the pointer that points to the next free location
      sub x7, x7, x13
      pop x8
      j Sucheflashpointer_Speicherbelegung_fertig # Finished

checksums: # Registerprüfsummen für x1 bis x15  Self-test register checksums for x1 - x15.
      .word             0x0000012F, 0x20000000, 0x24200133  #  x1 - x3   No checksum necessary for hardwired zero register x0
      .word 0x03244425, 0x79901867, 0x00000000, 0x09313607  #  x4 - x7
      .word 0x2442A08A, 0x03B031A0, 0x83942086, 0x00060001  #  x8 - x11
      .word 0x18000008, 0x272E0A20, 0x28602D5F, 0x3C292F5C  # x12 - x15

1:  andi x13, x11, Flag_ramallot & ~Flag_visible
    beq x13, zero, Sucheflashpointer_Speicherbelegung_fertig # Benötigt doch kein RAM.
      # writeln "Speicher gewünscht !"
      # Die Flags werden später nicht mehr gebraucht.
      # This one allocates RAM, Flags are not needed anymore.

      andi x11, x11, 0x0F # Das unterste Nibble maskieren  Mask lower 4 bits that contains amount of 64 bit locations requested.

 #     pushda x11
 #     call hexdot
 #     writeln " initram"

        # Bei Null Bytes brauche ich nichts zu kopieren, den Fall erkennt move.
        # Zero byte requests are handled by move itself, no need to catch this special case. Sounds strange, but is useful to have two handles for one variable.
        slli x11, x11, CELLSHIFT # Multiply by size of cell in bytes
        sub x7, x7, x11 # Ramvariablenpointer wandern lassen  Subtract from the pointer that points to the next free location

        # Den neu geschaffenen Platz initialisieren !
        # Initialise the freshly allocated RAM locations !

        # Muss zuerst schaffen, das Ende der aktuellen Definition zu finden.
        # Search for end of code of current definition.
        pushdatos
        addi x8, x8, 2*CELL
        call skipstring  # x8 zeigt nun an den Codebeginn des aktuellen Wortes.  x8 points to start of code of current definition
        call suchedefinitionsende # Advance pointer to end of code. This is detected by "bx lr" or "pop {pc}" opcodes.

        # x8 ist nun an der Stelle, wo die Initialisierungsdaten liegen. x8 now points to the location of the initialisation at the end of code of current definition.
        # Kopiere die gewünschte Zahl von r1 Bytes von [r0] an [r5]  Copy desired amount of r1 bytes from [r0] to [r5].
                   # Quelle  Source
        pushdadouble x7, x11
        #pushda x7 # Ziel    Target
        #pushda x11 # Anzahl an Bytes  Amount
        call move

Sucheflashpointer_Speicherbelegung_fertig:
  # Speicherbelegung und -initialisierung abgeschlossen.
  # Finished RAM allocation and initialisation.

  # Weiterhangeln  Continue crawl.
  call dictionarynext
  popda x10
  beq x10, zero, SucheFlashPointer_Hangelschleife

  laf x10, ZweitFadenende
  sc x8, 0(x10) # Das Fadenende für den Flash setzen.  Set pointer to latest definition.
  drop

  laf x10, VariablenPointer # Set pointer to current end-of-ram-dictionary for later permanent RAM allocations by variables defined in Flash.
  sc x7, 0(x10)


  # writeln "Hangelschleife durchgelaufen"

  # Mache mich auf die Suche nach dem Dictionarypointer im Flash:
  # Suche jetzt gleich noch den DictionaryPointer.
  # Time to search the Dictionarypointer !

  laf x10, FlashDictionaryEnde
  laf x11, FlashDictionaryAnfang
  li x12, erasedword

  # Gehe Rückwärts, bis ich aus dem $FFFF-Freigebiet in Daten komme.
  # Run backwards through whole Flash memory to find DictionaryPointer.
1:beq x10, x11, 2f #  Wenn ich am Anfang angelangt bin, ist das der DictionaryPointer.
                   #  Finished if beginning of Flash is hit.

  addi x10, x10, -CELL

  lc x13, 0(x10)

  beq x13, x12, 1b # Wenn es nicht gleich ist, habe ich eine Füllung gefunden.
                   # If there is not $FFFF on that location I have found "end of free space".

  addi x10, x10, CELL

2:# Dictionarypointer gefunden. Found DictionaryPointer.
  laf x11, ZweitDictionaryPointer # We start to compile into RAM - the pointer found goes to the second set of pointers that are swapped with compiletoflash/compiletoram.
  sc x10, 0(x11)

#  writeln "Dictionarypointer gefunden"

  .ifdef initflash
  call initflash
  .endif
