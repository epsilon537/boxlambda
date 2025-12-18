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

# This is the core of the forth core so to speak. Through the use of
# a number of clever macros such as Definition and CoreVariable, the assembler
# preprocessor stage generates a dictionary of core Words.
# First, the required macros and symbols are defined, then the dictionary of core words.
# When the assembler preprocssor reaches the end of this file, the core
# dictionary is completed.

.macro call destination
  jal \destination
.endm

.include "datastackandmacros.s"

# -----------------------------------------------------------------------------
#   Linker labels
# -----------------------------------------------------------------------------
.extern __forth_emem_start
.extern __forth_emem_end
.extern __forth_imem_start
.extern __forth_imem_end
.extern __datastack
.extern __datastack_end

# -----------------------------------------------------------------------------
#   Type of memory
# -----------------------------------------------------------------------------

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

  .equ writtencell, writtenword
  .equ erasedcell,  erasedword

# -----------------------------------------------------------------------------
#   Dictionary header macro
# -----------------------------------------------------------------------------

.macro Definition Flags, Name
    .balign CELL, 0
    .equ "Dictionary_\Name", .  # Labels for a more readable assembler listing only

9:  .word 9f          # Insert Link
    .word \Flags      # Flag field

    .byte 8f - 7f     # Calculate length of name field
7:  .ascii "\Name"    # Insert name string

8:  .balign 4, 0      # Realign

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

8:  .balign 4, 0      # Realign

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

.equ Flag_ramallot,   Flag_visible | 0x0080      # ramallot means that IMEM is reserved and initialised by catchflashpointers for this definition on startup
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

# For initialised variables at the end of RAM-Dictionary that are recognized by catchmempointers

.macro CoreVariable, Name #  Use the mechanism to obtain initialized variables.
  .set CoreVariablePointer, CoreVariablePointer - CELL
  .equ \Name, CoreVariablePointer
.endm

.macro DoubleCoreVariable, Name #  Use the mechanism to obtain initialized variables.
  .set CoreVariablePointer, CoreVariablePointer - 2*CELL
  .equ \Name, CoreVariablePointer
.endm

.macro ramallot Name, Amount      # For variables and buffers at the beginning of memory that are to be used in the kernel
  .equ \Name, rampointer           # uninitialized.
  .set rampointer, rampointer + \Amount
.endm

# -----------------------------------------------------------------------------
# Hardwired core variables, buffers and stacks at the beginning of memory
# -----------------------------------------------------------------------------
.set rampointer, __forth_imem_start  # Set location for core variables.

# Variables of core that are not visible
# Variables for Primary/Secondary dictionary management

ramallot Dictionarypointer, CELL        # These five variables need to be exactly in this order in memory.
ramallot SecondDictionarypointer, CELL   # Dictionarypointer +  4
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

.equ Numberbufferlength, 18*CELL-1 # Number buffer (Length+1 mod CELL = 0)
ramallot Numberbuffer, Numberbufferlength+1 # Reserviere mal großzügig 72 Bytes RAM für den Numberbuffer

.ifndef tiblength
  .equ tiblength, 200
.endif

  # When forth_init is called from C, the gp, tp and sp are saved in this
  # locationa.
  ramallot gp_tp_sp, 3*CELL

.equ Maximuminput,   tiblength        # Input buffer for an Address-Length string
ramallot Inputbuffer, Maximuminput

.equ EmemDictionaryStart, __forth_emem_start # Start of EMEM dictionary, __forth_emem_start is a linker variable.
.equ ImemDictionaryStart, rampointer # Start of IMEM dictionary
.equ ImemDictionaryEnd, __forth_imem_end    # End of IMEM dictionary. __forth_imem_end is a linker variable.

# -----------------------------------------------------------------------------
#  Macros for "typesetting" :-)
# -----------------------------------------------------------------------------

.macro write Notification
  call dotquote
        .byte 8f - 7f         # Compute length of string.
7:      .ascii "\Notification"

8:  .balign 4, 0      # Realign

.endm

.macro writeln Notification
  call dotquote
        .byte 8f - 7f         # Compute length of string.
.ifdef crlf
7:      .ascii "\Notification\n\r"
.else
7:      .ascii "\Notification\n"
.endif

8:  .balign 4, 0      # Realign

.endm

.macro welcome Notification Notification2
  call dotquote
        .byte 8f - 7f         # Compute length of string.
.ifdef crlf
7:      .ascii "\n\r\Mecrisp-Quintus 1.1.1\Notification\n\r\Notification2\n\r"
.else
7:      .ascii "\nMecrisp-Quintus 1.1.1\Notification\n\Notification2\n"
.endif

8:  .balign 4, 0      # Realign

.endm

# -----------------------------------------------------------------------------
# Preparations for dictionary structure
# -----------------------------------------------------------------------------
.balign CELL, 0
CoreDictionaryStart: # Set entry point for Dictionary
# Variables defined in the core words are placed at the end of IMEM
# SetCoreVariablePointer advances down, to lower addresses.
.set CoreVariablePointer, ImemDictionaryEnd

  Definition Flag_invisible, "--- Mecrisp-Quintus 1.1.1 ---"

# .include "flash.s"

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

.ifdef softwaremultiply
.include "../common/multiplydivide-sw.s"
.else
.include "multiplydivide.s"
.endif

.include "double.s"
.include "memory.s"
.include "c-ffi.s"

# -----------------------------------------------------------------------------
# Finalize the core dictionary structure and put a pointer
# -----------------------------------------------------------------------------

  Definition_EndOfCore Flag_invisible, "--- End of Core ---"

# -----------------------------------------------------------------------------
#  End of Dictionary
# -----------------------------------------------------------------------------


