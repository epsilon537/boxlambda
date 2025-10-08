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

.include "datastackandmacros.s"

# -----------------------------------------------------------------------------
#   Linker labels
# -----------------------------------------------------------------------------
.extern __forth_ram_start
.extern __forth_ram_end
.extern __forth_imem_start
.extern __forth_imem_end
.extern __datastack
.extern __datastack_end

# -----------------------------------------------------------------------------
#   Type of memory
# -----------------------------------------------------------------------------

.ifndef erasedflashspecial
  .ifdef erasedmemcontainszero
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

# This macro is used once, by the final core word.
.macro Definition_EndOfCore Flags, Name
    .balign CELL, 0
    .equ "Dictionary_\Name", .  # Labels for a more readable assembler listing only

9:  .word 0xffffffff  # Indicates end of dictionary

    .word \Flags      # Flag field

    .byte 8f - 7f     # Calculate length of name field
7:  .ascii "\Name"    # Insert name string

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

    .equ "Code_\Name", .        # Labels for a more readable assembler listing only
.endm

.ifdef erasedmemcontainszero
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

.equ Flag_opcodable, Flag_visible | 0x200
.equ Flag_undefined,   Flag_visible | 0xBD4A3BC0

# Allows to keep the link register without saving it to the stack.

.equ Flag_noframe,     Flag_visible | 0x400

# -----------------------------------------------------------------------------
# Macros for building dictionary
# -----------------------------------------------------------------------------

# For initialised variables at the end of RAM-Dictioanary that are recognized by catchmempointers

.macro CoreVariable, Name #  Use the mechanism to obtain initialized variables.
  .set CoreVariablePointer, CoreVariablePointer - CELL
  .equ \Name, CoreVariablePointer
.endm

.macro DoubleCoreVariable, Name #  Use the mechanism to obtain initialized variables.
  .set CoreVariablePointer, CoreVariablePointer - 2*CELL
  .equ \Name, CoreVariablePointer
.endm

.macro ramallot Name, Amount        # For variables and buffers at the beginning of the RAM that are to be used in the kernel
  .equ \Name, rampointer            # uninitialized.
  .set rampointer, rampointer + \Amount
.endm

# -----------------------------------------------------------------------------
# Hardwired core variables, buffers and stacks at the begin of RAM
# -----------------------------------------------------------------------------
.set rampointer, __forth_ram_start  # Set location for core variables.

# Variables of core that are not visible
# Variables for IMEM/Second dictionary management

ramallot Dictionarypointer, CELL        # These five variables need to be exactly in this order in memory.
ramallot SecondDictionaryPointer, CELL   # Dictionarypointer +  4
ramallot ThreadEnd, CELL                # Dictionarypointer +  8
ramallot SecondThreadEnd, CELL           # Dictionarypointer + 12
ramallot VariablesPointer, CELL         # Dictionarypointer + 16

ramallot constantfoldingpointer, CELL
ramallot leavepointer, CELL
ramallot Entrypoint, CELL

ramallot FlashFlags, CELL

.ifdef within_os # Specials for Linux targets
  ramallot arguments, CELL
.endif

.equ Numberbufferlength, 18*CELL-1 # Numberbufferlänge+1 sollte durch Zellengröße teilbar sein !      Number buffer (Length+1 mod CELL = 0)
ramallot Numberbuffer, Numberbufferlength+1 # Reserviere mal großzügig 72 Bytes RAM für den Numberbuffer

.ifndef tiblength
  .equ tiblength, 200
.endif

  # When Forth is called from C, sp is saved in returnstactstart
  # i.e. the sp at Forth entry is considered the start of the return stack.
  # The reset and quit words reset sp back to this value.
  ramallot returnstackstart, CELL

  # When forth_init is called from C, the gp and tp are saved in these two
  # locations
  ramallot generalpointer, CELL
  ramallot threadpointer, CELL

.ifdef dualcore
  ramallot datastackcore1end, datastacklength  # Data stack
  ramallot datastackcore1begin, 0

  ramallot returnstackcore1end, returnstacklength  # Return stack
  ramallot returnstackcore1begin, 0

  ramallot trampolineaddr, 4
.endif

.equ Maximuminput,   tiblength        # Input buffer for an Address-Length string
ramallot Inputbuffer, Maximuminput  # Inputbuffer wird einen Adresse-Länge String enthalten

.ifdef flash8bytesblockwrite
  .equ Sammelstellen, 32 # 32 * (8 + 4) = 384 Bytes
  ramallot Sammeltabelle, Sammelstellen * 12 # Buffer 32 blocks of 8 bytes each for ECC constrained Flash write
.endif

.equ RamDictionaryStart, rampointer # Start of RAM dictionary
.equ RamDictionaryEnd,   __forth_ram_end    # End of RAM dictionary


# -----------------------------------------------------------------------------
#  Macros for "typesetting" :-)
# -----------------------------------------------------------------------------

.macro write Notification
  call dotquote
        .byte 8f - 7f         # Compute length of string.
7:      .ascii "\Notification"

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

.endm

.macro writeln Notification
  call dotquote
        .byte 8f - 7f         # Compute length of string.
.ifdef crlf
7:      .ascii "\Notification\n\r"
.else
7:      .ascii "\Notification\n"
.endif

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

.endm

.macro welcome Notification Notification2
  call dotquote
        .byte 8f - 7f         # Compute length of string.
.ifdef crlf
7:      .ascii "\n\r\Mecrisp-Quintus 1.1.1\Notification\n\r\Notification2\n\r"
.else
7:      .ascii "\nMecrisp-Quintus 1.1.1\Notification\n\Notification2\n"
.endif

.ifdef compressed_isa
8:  .balign 2, 0      # Realign
.else
8:  .balign 4, 0      # Realign
.endif

.endm

# -----------------------------------------------------------------------------
# Preparations for dictionary structure
# -----------------------------------------------------------------------------
.balign CELL, 0
CoreDictionaryStart: # Set entry point for Dictionary
# Variables defined in the core words are placed at the end of RAM
# SetCoreVariablePointer advances down, to lower addresses.
.set CoreVariablePointer, RamDictionaryEnd

  Definition Flag_invisible, "--- Mecrisp-Quintus 1.1.1 ---"

.include "flash.s"

.ifdef flash8bytesblockwrite # Needs to be at the beginning for proper ifdef detection
.include "../common/flash8bytesblockwrite.s"
.endif

.include "stackjugglers.s"

.ifdef letsroll
.include "roll.s"
.endif

.include "comparisions.s"
.include "calculations.s"

.include "terminal.s"

.include "query.s"
.include "strings.s"
.include "deepinsight.s"
.include "token.s"
.include "buildsdoes.s"
.include "compiler.s"
.include "compiler-memory.s"
.include "doloop.s"
.include "case.s"
.include "controlstructures.s"
.include "logic.s"
.include "interpreter.s"
.include "numberstrings.s"
.include "numberoutput.s"

.ifdef mipscore
.include "../common/multiplydivide-mips.s"
.else
  .ifdef softwaremultiply
  .include "../common/multiplydivide-sw.s"
  .else
  .include "multiplydivide.s"
  .endif
.endif

.include "double.s"
.include "memory.s"

# -----------------------------------------------------------------------------
# Finalize the dictionary structure and put a pointer into IMEM-Dictionary
# -----------------------------------------------------------------------------

  Definition_EndOfCore Flag_invisible, "--- End of Core ---"

# -----------------------------------------------------------------------------
#  End of Dictionary
# -----------------------------------------------------------------------------


