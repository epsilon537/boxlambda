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

.ifdef mipscore

.else
  .macro call destination
    jal \destination
  .endm
.endif

.include "../common/datastackandmacros.s"

# -----------------------------------------------------------------------------
#   Type of flash memory
# -----------------------------------------------------------------------------

.ifndef erasedflashspecial
  .ifdef erasedflashcontainszero
    .equ erasedbyte, 0
    .equ erasedhalfword, 0
    .equ erasedword, 0
    .equ eraseddword, 0

    .equ writtenhalfword, 0xFFFF
    .equ writtenword,     0xFFFFFFFF
    .equ writtendword,    0xFFFFFFFFFFFFFFFF
  .else
    .equ erasedbyte,     0xFF
    .equ erasedhalfword, 0xFFFF
    .equ erasedword,     0xFFFFFFFF
    .equ eraseddword,    0xFFFFFFFFFFFFFFFF

    .equ writtenhalfword, 0
    .equ writtenword, 0
    .equ writtendword, 0
  .endif
.endif

.ifdef RV64
  .equ writtencell, writtendword
  .equ erasedcell,  eraseddword
.else
  .equ writtencell, writtenword
  .equ erasedcell,  erasedword
.endif

# -----------------------------------------------------------------------------
#   Dictionary header macro
# -----------------------------------------------------------------------------

.macro Definition Flags, Name
    .balign CELL, 0
    .equ "Dictionary_\Name", .  # Labels for a more readable assembler listing only

    .ifdef RV64
9:  .dword 9f         # Insert Link
    .dword \Flags     # Flag field
    .else
9:  .word 9f          # Insert Link
    .word \Flags      # Flag field
    .endif

    .byte 8f - 7f     # Calculate length of name field
7:  .ascii "\Name"    # Insert name string

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

    .equ "Code_\Name", .        # Labels for a more readable assembler listing only
.endm


.macro Definition_EndOfCore Flags, Name
    .balign CELL, 0
    .equ "Dictionary_\Name", .  # Labels for a more readable assembler listing only

    .ifdef RV64
9:  .dword FlashDictionaryAnfang
    .dword \Flags
    .else

     .ifdef flash8bytesblockwrite
9:      .word FlashDictionaryAnfang + 0x04 # Insert Link with offset because of alignment issues.
     .else
9:      .word FlashDictionaryAnfang        # Link einfügen  Insert Link
     .endif

    .word \Flags      # Flag field
    .endif

    .byte 8f - 7f     # Calculate length of name field
7:  .ascii "\Name"    # Insert name string

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

    .equ "Code_\Name", .        # Labels for a more readable assembler listing only
.endm

.ifdef erasedflashcontainszero
  .equ Flag_invisible,  0              # Erased Flash needs to give invisible Flags.
  .equ Flag_visible,    1 << SIGNSHIFT # 0x80000000
.else
  .equ Flag_invisible, -1
  .equ Flag_visible,    0
.endif


.equ Flag_immediate,  Flag_visible | 0x0010
.equ Flag_inline,     Flag_visible | 0x0020
.equ Flag_immediate_compileonly, Flag_visible | 0x0030 # Immediate + Inline

.equ Flag_ramallot,   Flag_visible | 0x0080      # Ramallot means that RAM is reserved and initialised by catchflashpointers for this definition on startup
.equ Flag_variable,   Flag_ramallot| 1           # How many 32 bit locations shall be reserved ?
.equ Flag_2variable,  Flag_ramallot| 2

.equ Flag_foldable,   Flag_visible | 0x0040 # Foldable when given number of constants are available.
.equ Flag_foldable_0, Flag_visible | 0x0040
.equ Flag_foldable_1, Flag_visible | 0x0041
.equ Flag_foldable_2, Flag_visible | 0x0042
.equ Flag_foldable_3, Flag_visible | 0x0043
.equ Flag_foldable_4, Flag_visible | 0x0044
.equ Flag_foldable_5, Flag_visible | 0x0045
.equ Flag_foldable_6, Flag_visible | 0x0046
.equ Flag_foldable_7, Flag_visible | 0x0047

.equ Flag_buffer, Flag_visible | 0x0100
.equ Flag_buffer_foldable, Flag_buffer|Flag_foldable

# Different from Mecrisp-Stellaris ! Opcodability is independent of constant folding flags in Mecrisp-Quintus.
# This greatly simplifies this optimisation.

.equ Flag_opcodierbar, Flag_visible | 0x200
.equ Flag_undefined,   Flag_visible | 0xBD4A3BC0

# Allows to keep the link register without saving it to the stack.

.equ Flag_noframe,     Flag_visible | 0x400

# -----------------------------------------------------------------------------
# Makros zum Bauen des Dictionary
# Macros for building dictionary
# -----------------------------------------------------------------------------

# Für initialisierte Variablen am Ende des RAM-Dictionary
# For initialised variables at the end of RAM-Dictioanary that are recognized by catchflashpointers

.macro CoreVariable, Name #  Benutze den Mechanismus, um initialisierte Variablen zu erhalten.
  .set CoreVariablenPointer, CoreVariablenPointer - CELL
  .equ \Name, CoreVariablenPointer
.endm

.macro DoubleCoreVariable, Name #  Benutze den Mechanismus, um initialisierte Variablen zu erhalten.
  .set CoreVariablenPointer, CoreVariablenPointer - 2*CELL
  .equ \Name, CoreVariablenPointer
.endm

.macro ramallot Name, Menge         # Für Variablen und Puffer zu Beginn des Rams, die im Kern verwendet werden sollen.
  .equ \Name, rampointer            # Uninitialisiert.
  .set rampointer, rampointer + \Menge
.endm

# -----------------------------------------------------------------------------
# Festverdrahtete Kernvariablen, Puffer und Stacks zu Begin des RAMs
# Hardwired core variables, buffers and stacks at the begin of RAM
# -----------------------------------------------------------------------------

.set rampointer, RamAnfang  # Ram-Anfang setzen  Set location for core variables.

# Variablen des Kerns  Variables of core that are not visible
# Variablen für das Flashdictionary  Variables for Flash management

ramallot Dictionarypointer, CELL        # These five variables need to be exactly in this order in memory.
ramallot ZweitDictionaryPointer, CELL   # Dictionarypointer +  4
ramallot Fadenende, CELL                # Dictionarypointer +  8
ramallot ZweitFadenende, CELL           # Dictionarypointer + 12
ramallot VariablenPointer, CELL         # Dictionarypointer + 16

ramallot konstantenfaltungszeiger, CELL
ramallot leavepointer, CELL
ramallot Einsprungpunkt, CELL

ramallot FlashFlags, CELL

.ifdef within_os # Specials for Linux targets
  ramallot arguments, CELL
.endif

.equ Zahlenpufferlaenge, 18*CELL-1 # Zahlenpufferlänge+1 sollte durch Zellengröße teilbar sein !      Number buffer (Length+1 mod CELL = 0)
ramallot Zahlenpuffer, Zahlenpufferlaenge+1 # Reserviere mal großzügig 72 Bytes RAM für den Zahlenpuffer

.ifndef datastacklength
  .equ datastacklength, 128*CELL
.endif
.ifndef returnstacklength
  .equ returnstacklength, 128*CELL
.endif
.ifndef tiblength
  .equ tiblength, 200
.endif

  ramallot datenstackende, datastacklength  # Data stack
  ramallot datenstackanfang, 0

  ramallot returnstackende, returnstacklength  # Return stack
  ramallot returnstackanfang, 0

.ifdef dualcore
  ramallot datastackcore1end, datastacklength  # Data stack
  ramallot datastackcore1begin, 0

  ramallot returnstackcore1end, returnstacklength  # Return stack
  ramallot returnstackcore1begin, 0

  ramallot trampolineaddr, 4
.endif

.equ Maximaleeingabe,   tiblength        # Input buffer for an Address-Length string
ramallot Eingabepuffer, Maximaleeingabe  # Eingabepuffer wird einen Adresse-Länge String enthalten

.ifdef flash8bytesblockwrite
  .equ Sammelstellen, 32 # 32 * (8 + 4) = 384 Bytes
  ramallot Sammeltabelle, Sammelstellen * 12 # Buffer 32 blocks of 8 bytes each for ECC constrained Flash write
.endif

.equ RamDictionaryAnfang, rampointer # Ende der Puffer und Variablen ist Anfang des Ram-Dictionary.  Start of RAM dictionary
.equ RamDictionaryEnde,   RamEnde    # Das Ende vom Dictionary ist auch das Ende vom gesamten Ram.   End of RAM dictionary = End of RAM


# -----------------------------------------------------------------------------
#  Macros for "typesetting" :-)
# -----------------------------------------------------------------------------

.macro write Meldung
  call dotgaensefuesschen
        .byte 8f - 7f         # Compute length of string.
7:      .ascii "\Meldung"

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

.endm

.macro writeln Meldung
  call dotgaensefuesschen
        .byte 8f - 7f         # Compute length of string.
.ifdef crlf
7:      .ascii "\Meldung\n\r"
.else
7:      .ascii "\Meldung\n"
.endif

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

.endm

.macro welcome Meldung
  call dotgaensefuesschen
        .byte 8f - 7f         # Compute length of string.
.ifdef crlf
7:      .ascii "Mecrisp-Quintus 1.1.1\Meldung\n\r"
.else
7:      .ascii "Mecrisp-Quintus 1.1.1\Meldung\n"
.endif

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

.endm

# -----------------------------------------------------------------------------
# Vorbereitung der Dictionarystruktur
# Preparations for dictionary structure
# -----------------------------------------------------------------------------
.balign CELL, 0
CoreDictionaryAnfang: # Dictionary-Einsprungpunkt setzen
                      # Set entry point for Dictionary

.set CoreVariablenPointer, RamDictionaryEnde # Im Flash definierte Variablen kommen ans RAM-Ende
                                             # Variables defined in Flash are placed at the end of RAM

  Definition Flag_invisible, "--- Mecrisp-Quintus 1.1.1 ---"

.include "flash.s"

.ifdef flash8bytesblockwrite # Needs to be at the beginning for proper ifdef detection
.include "../common/flash8bytesblockwrite.s"
.endif

.include "../common/stackjugglers.s"

.ifdef letsroll
.include "../common/roll.s"
.endif

.include "../common/comparisions.s"
.include "../common/calculations.s"

.include "terminal.s"

.include "../common/query.s"
.include "../common/strings.s"
.include "../common/deepinsight.s"
.include "../common/token.s"
.include "../common/buildsdoes.s"
.include "../common/compiler.s"
.include "../common/compiler-flash.s"
.include "../common/doloop.s"
.include "../common/case.s"
.include "../common/controlstructures.s"
.include "../common/logic.s"
.include "../common/interpreter.s"
.include "../common/numberstrings.s"
.include "../common/numberoutput.s"

.ifdef mipscore
.include "../common/multiplydivide-mips.s"
.else
  .ifdef softwaremultiply
  .include "../common/multiplydivide-sw.s"
  .else
  .include "../common/multiplydivide.s"
  .endif
.endif

.include "../common/double.s"
.include "../common/memory.s"

# -----------------------------------------------------------------------------
# Schließen der Dictionarystruktur und Zeiger ins Flash-Dictionary
# Finalize the dictionary structure and put a pointer into changeable Flash-Dictionary
# -----------------------------------------------------------------------------

  Definition_EndOfCore Flag_invisible, "--- Flash Dictionary ---"

# -----------------------------------------------------------------------------
#  End of Dictionary
# -----------------------------------------------------------------------------


