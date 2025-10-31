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

# Besondere Teile des Compilers, die mit der Dictionarystruktur im Flash zu tun haben.
# Special parts of compiler tightly linked with generating code for Flash memory.


# -----------------------------------------------------------------------------
  Definition Flag_visible, "smudge" # ( -- )
smudge:
# -----------------------------------------------------------------------------
  push x1

  call align4komma # Align on 4 to make sure the last compressed opcode to fullfill ANS requirement.

  .ifdef RV64
    call align8komma
  .endif

  # -----------------------------------------------------------------------------
  # Smudge for RAM (both IMEM and EMEM)

smudge_ram:
  pushdaconst Flag_visible
  j setflags_intern

# -----------------------------------------------------------------------------
  Definition Flag_visible, "setflags" # ( x -- )
setflags:
# -----------------------------------------------------------------------------

  li x14, (Flag_undefined >> 5) & 0xFFFFFFFF # Sichtbarkeit entfernen  Remove visibility
  bne x14, x8, 1f                            # Flag gültig ?           Flag valid ?

    la x8, checksums
    j dotnfuesschen

1:push x1 # Flag ok.

setflags_intern:
  # -----------------------------------------------------------------------------
  # Setflags for RAM
setflags_ram:

  # Hole die Flags des aktuellen Wortes   Fetch flags of current definition
  laf x14, ThreadEnd # Variable containing pointer to current definition
  lc x14,    0(x14)  # Address of current definition
  lc x15, CELL(x14)  # Flags des zuletzt definierten Wortes holen  Fetch its Flags

  push x10
  li x10, -1
  bne x15, x10, 1f
    mv x15, x8     # Direkt setzen, falls an der Stelle noch -1 steht  Set directly, if there are no Flags before
1:  or x15, x15, x8 # Hinzuverodern, falls schon Flags da sind          If there already are Flags, OR them together.
  pop x10

  sc x15, CELL(x14)
  drop
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_1, "aligned" # ( c-addr -- a-addr )
aligned:
# -----------------------------------------------------------------------------
  andi x15, x8, 1
  add x8, x8, x15
  andi x15, x8, 2
  add x8, x8, x15

  .ifdef RV64
  andi x15, x8, 4
  add x8, x8, x15
  .endif

  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "here" # ( -- addr ) Gives Dictionarypointer
here: # Gibt den Dictionarypointer zurück
# -----------------------------------------------------------------------------
  pushdatos    # Platz auf dem Datenstack schaffen
  laf x8, Dictionarypointer
  lc x8, 0(x8) # Hole den Dictionarypointer
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "ramvar-here" # ( -- a-addr ) Gives RAM management pointer
ramvarhere:
# -----------------------------------------------------------------------------
  pushdatos
  laf x8, VariablesPointer
  lc x8, 0(x8)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "addrinimem?" # ( a-addr -- a-addr )
addrinimem:
# -----------------------------------------------------------------------------
  laf x14, __forth_imem_start
  laf x15, __forth_imem_end
  j 1f

# -----------------------------------------------------------------------------
  Definition Flag_visible, "addrinram?" # ( a-addr -- a-addr )
# -----------------------------------------------------------------------------
  laf x14, __forth_ram_start
  laf x15, __forth_ram_end

1:bltu x8, x14, 2f
  bgeu x8, x15, 2f

    li x8, -1
    ret

2:li x8, 0
  ret

  .ifdef flash8bytesblockwrite
# -----------------------------------------------------------------------------
  Definition Flag_visible, "align8," # ( -- )
align8komma: # Macht den Dictionarypointer auf 8 gerade
# -----------------------------------------------------------------------------
  push x1

  call align4komma

  call here
  andi x8, x8, 4
  beq x8, zero, 1f

    li x8, 0x00000013 # nop Opcode
    call wkomma
    j 2f

1:drop
2:pop x1
  ret
  .endif

  .ifdef compressed_isa

.ifdef RV64
# -----------------------------------------------------------------------------
  Definition Flag_visible, "align" # ( -- )
align8komma: # Macht den Dictionarypointer auf 8 gerade
# -----------------------------------------------------------------------------
  push x1

  call align4komma

  call here
  andi x8, x8, 4
  beq x8, zero, 1f

    li x8, 0x00000013 # nop Opcode
    call wkomma
    j 2f

1:drop
2:pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "align4" # ( -- )
align4komma: # Macht den Dictionarypointer auf 4 gerade
# -----------------------------------------------------------------------------
.else
# -----------------------------------------------------------------------------
  Definition Flag_visible, "align" # ( -- )
align4komma: # Macht den Dictionarypointer auf 4 gerade
# -----------------------------------------------------------------------------
.endif

  push x1

  call here

  # Alignment for - non-existent - c, which may happen when user allots non-aligned amounts of memory
  andi x15, x8, 1
  add x8, x8, x15
  laf x14, Dictionarypointer
  sc x8, 0(x14)

  # Alignment for h,
  andi x8, x8, 2
  beq x8, zero, 1f

    li x8, 0x0001 # Compressed NOP opcode
    call hkomma
    j 2f

1:drop
2:pop x1
  ret

# -----------------------------------------------------------------------------
kommasomewhere: # ( x addr -- ) For backpatching of jump opcodes.
# -----------------------------------------------------------------------------
  push x1
  ddup

  call hkommasomewhere

  swap
  srli x8, x8, 16
  swap
  addi x8, x8, 2
  pop x1
hkommasomewhere: # ( x addr -- ) For backpatching of jump opcodes.
  push x1
  dup
  j hkomma_intern

# -----------------------------------------------------------------------------
  Definition Flag_visible, "h," # ( -- )
hkomma: # Fügt 16 Bits an das Dictionary an
# -----------------------------------------------------------------------------
  push x1

  call here
  dup

  pushdaconst 2
  call allot

hkomma_intern:
  drop

hkomma_ram:
  # Simply write directly if compiling for RAM.
  popda x15
  sh x8, 0(x15)
  drop

1:pop x1
  ret


.ifdef RV64
# -----------------------------------------------------------------------------
  Definition Flag_visible, "w," # ( x -- )
wkomma: # Fügt 32 Bits an das Dictionary an  Write 32 bits in Dictionary
# -----------------------------------------------------------------------------
.else
# -----------------------------------------------------------------------------
  Definition Flag_visible, "," # ( x -- )
wkomma: # Fügt 32 Bits an das Dictionary an  Write 32 bits in Dictionary
cellkomma:
# -----------------------------------------------------------------------------
.endif

  push x1
  dup
  call hkomma
  srli x8, x8, 16
  pop x1
  j hkomma

  .else # compressed_isa

.ifdef RV64
# -----------------------------------------------------------------------------
  Definition Flag_visible, "align" # ( -- )
align8komma: # Macht den Dictionarypointer auf 8 gerade
# -----------------------------------------------------------------------------
  push x1

  call align4komma

  call here
  andi x8, x8, 4
  beq x8, zero, 1f

    # li x8, writtencell
    li x8, 0x00000013 # nop Opcode
    call wkomma
    j 2f

1:drop
2:pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "align4" # ( -- )
align4komma: # Macht den Dictionarypointer auf 4 gerade
# -----------------------------------------------------------------------------
.else
# -----------------------------------------------------------------------------
  Definition Flag_visible, "align" # ( -- )
align4komma: # Macht den Dictionarypointer auf 4 gerade
# -----------------------------------------------------------------------------
.endif

  push x1  # Eigentlich nichts zu tun, vom Kern aus immer auf 4 gerade.
           # Es könnte aber sein dass der Benutzer ein allot verwendet hat.
  call here

  andi x15, x8, 1
  add x8, x8, x15
  andi x15, x8, 2
  add x8, x8, x15

  laf x14, Dictionarypointer
  sc x8, 0(x14)
  drop

  pop x1
  ret

# -----------------------------------------------------------------------------
kommasomewhere: # ( x addr -- ) For backpatching of jump opcodes.
# -----------------------------------------------------------------------------
  push x1
  dup
  j wkomma_intern

.ifdef RV64
# -----------------------------------------------------------------------------
  Definition Flag_visible, "w," # ( x -- )
wkomma: # Write 32 bits in Dictionary
# -----------------------------------------------------------------------------
.else
# -----------------------------------------------------------------------------
  Definition Flag_visible, "," # ( x -- )
wkomma: # Write 32 bits in Dictionary
cellkomma:
# -----------------------------------------------------------------------------
.endif

  push x1

#  write "Komma: "
#  call here
#  call hexdot
#  dup
#  call hexdot
#  writeln ""

  call here
  dup
  call four_allot

wkomma_intern:
  drop
komma_ram:
  # Simply write directly
  popda x15
  sw x8, 0(x15)
  drop

1:pop x1
  ret

  .endif # compressed_isa

  .ifdef RV64

# -----------------------------------------------------------------------------
  Definition Flag_visible, "," # ( x -- )
cellkomma: # Fügt 64 Bits an das Dictionary an  Write 64 bits in Dictionary
# -----------------------------------------------------------------------------
  push x1
  dup
  call wkomma
  srli x8, x8, 32
  pop x1
  j wkomma

  .endif

#------------------------------------------------------------------------------
  Definition Flag_visible, "allot" # Advance Dictionarypointer and check if there is enough space left for the requested amount.
allot:
#------------------------------------------------------------------------------

#   # Simple variant without any checks.
#   li x14, Dictionarypointer
#   lc x15, 0(x14)
#   add x15, x15, x8
#   sc x15, 0(x14)
#   drop
#   ret

  push_x1_x10

  popda x10 # Requested length

  call compiletoramq
  popda x15
  bne x15, zero, allot_ram

allot_imem:
  laf x14, Dictionarypointer
  lc x15, 0(x14)
  add x15, x15, x10

  laf x10, __forth_imem_end

  bltu x15, x10, 2f
    writeln "Forth IMEM full"
    j quit

allot_ram:
  call ramvarhere  # There are variables defined at the end of RAM. Don't overwrite them !
  laf x14, Dictionarypointer
  lc x15, 0(x14)
  add x15, x15, x10

  bltu x15, x8, 1f
    writeln "Ram full"
    j quit

1:drop

2:sc x15, 0(x14)
  pop_x1_x10
  ret

four_allot:
  pushdaconst 4
  j allot

# There are two sets of Pointers: One set for RAM, one set for IMEM Dictionary.
# They are exchanged if you want to write to the "other" memory type.
# A small check takes care of the case if you are already in the memory you request.

# -----------------------------------------------------------------------------
  Definition Flag_visible, "forgetram"
# -----------------------------------------------------------------------------
  push x1
  call compiletoram

  # Set dictionary pointer into RAM first
  laf x14, Dictionarypointer
  laf x15, RamDictionaryStart
  sc x15, 0(x14)

  # Set latest for RAM
  laf x14, ThreadEnd
  la x15, CoreDictionaryStart
  sc x15, 0(x14)

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "compiletoram?"
compiletoramq:
# -----------------------------------------------------------------------------
  push x1
  call here
  call addrinimem
  inv x8
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "compiletoram"
compiletoram:
# -----------------------------------------------------------------------------
  push x1
  call compiletoramq

  popda x15
  bne x15, zero, 2f

  # I am in IMEM. Check for collisions between the variables and the RAM dictionary and switch!
  laf x14, Dictionarypointer
  lc x15, 1*CELL(x14) # SecondDictionaryPointer
  lc x14, 4*CELL(x14) # VariablesPointer

  bltu x15, x14, Secondpointerswap
   writeln " Variables collide with dictionary"
Secondpointerswap:

  pushdatos

    laf x8, Dictionarypointer

    lc x14, 0*CELL(x8) # Dictionarypointer
    lc x15, 1*CELL(x8) # SecondDictionaryPointer
    sc x14, 1*CELL(x8)
    sc x15, 0*CELL(x8)

    # In BoxLambda, only to the DictionaryPointers are swapped so
    # new entries are compiled into IMEM or EMEM (RAM) as needed, but all words
    # remain linked in one single dictionary
    #
    # lc x14, 2*CELL(x8) # ThreadEnd
    # lc x15, 3*CELL(x8) # SecondThreadEnd
    # sc x14, 3*CELL(x8)
    # sc x15, 2*CELL(x8)

  li x8, 0 # Trick: Allot will check for collisions, too, and reports RAM full then.
  pop x1
  j allot

# -----------------------------------------------------------------------------
  Definition Flag_visible, "compiletoimem"
compiletoimem:
# -----------------------------------------------------------------------------
  push x1
  call compiletoramq

  popda x15
  bne x15, zero, Secondpointerswap # I'm in RAM, switch!

2:pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_0, "(latest)"
# -----------------------------------------------------------------------------
  pushdatos
  laf x8, ThreadEnd
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_0, "(dp)"
# -----------------------------------------------------------------------------
  pushdatos
  laf x8, Dictionarypointer
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(create)"
create:
# -----------------------------------------------------------------------------
  push x1
  call token # Fetch name for new definition.
  # ( Tokenaddress Length )
  bne x8, zero, 1f

    # Check if token is empty. That happens if input buffer is empty after create.
    writeln " Create needs name !"
    j quit

1:# Name is ok.
  # Check if it already exists.
  ddup
  # ( Tokenadress Length Tokenaddress Length )
  call find
  # ( Tokenaddress Length Entryaddress Flags )
  drop # No need for the Flags...
  # ( Tokenaddress Length Entryaddress )

  # Do we have a search result ?
  popda x15
  beq x15, zero, 2f
  # ( Tokenaddress Length )
    ddup
    write "Redefine "
    call type # Display the new token name again
    write ". "
2:

  call align4komma

  .ifdef RV64
    call align8komma
  .endif


  # -----------------------------------------------------------------------------
  # Create for RAM
create_ram:
  # ( Tokenaddress Length )

  call here

    # Write Link
    pushdaaddrf ThreadEnd
    lc x8, 0(x8)
    call cellkomma # Old latest

  laf x14, ThreadEnd # Set new latest
  sc x8, 0(x14)
  drop

  # Set initial Flags to Invisible.
  pushdaconst Flag_invisible
  call cellkomma

  # Write Name
  call stringkomma

create_end: # Save code entry point of current definition for recurse and dodoes

  call here
  laf x14, Entrypoint
  sc x8, 0(x14)
  drop

  pop x1
  ret


# -----------------------------------------------------------------------------
  Definition Flag_visible, "variable" # ( n -- )
# -----------------------------------------------------------------------------
  pushdaconst 1
  j nvariable

# -----------------------------------------------------------------------------
  Definition Flag_visible, "2variable" # ( d -- )
# -----------------------------------------------------------------------------
  pushdaconst 2
  j nvariable

#------------------------------------------------------------------------------
  Definition Flag_visible, "nvariable" # ( Init-Values Length -- )
nvariable: # Creates an initialised variable of given length.
#------------------------------------------------------------------------------
  push_x1_x10_x11
  call create

variable_ram:
  call variable_buffer_ram_prepare

  popda x10 # Amount of cells to write is in TOS.
  beq x10, zero, variable_buffer_ram_finalise # If nvariable is called with length zero... Maybe this could be useful sometimes.

1:call cellkomma
  addi x10, x10, -1
  bne x10, zero, 1b

  j variable_buffer_ram_finalise

#------------------------------------------------------------------------------
  Definition Flag_visible, "buffer:" # ( Length -- )
  # Creates an uninitialised buffer of given bytes length.
#------------------------------------------------------------------------------
  push_x1_x10_x11

  call aligned # Round requested buffer length to next 4-Byte boundary to ensure alignment

  call create

buffer_ram:
  call variable_buffer_ram_prepare
  call allot
variable_buffer_ram_finalise:
  # Variables always are 0-foldable as their address never changes.
  pushdaconst Flag_ramallot & ~Flag_visible # For better detection of variables and buffers in dictionary
  call setflags
pop_x1_x10_x11_smudge:
  pop_x1_x10_x11
  j smudge

# -----------------------------------------------------------------------------

variable_buffer_ram_prepare:
  push x1

  # This is simple: Write code, write value, a classic Forth variable.

  .ifdef compressed_isa
    # This is to align dictionary pointer to have variable locations that are always 4-even
    call align4komma
  .endif

  call dup_komma

  .ifdef mipscore

  pushdaconst 0x03E00009 | reg_tos << 11 # jalr x8, x1
  call wkomma
  pushdaconst 0x00000000 # nop
  pop x1
  j wkomma

  .else

  .ifdef RV64

    # Auf 8-ungerade ausrichten, damit es mit dem jalr-Opcode dann stimmt.
    call here
    andi x8, x8, 4
    bne x8, zero, 1f
      pushdaconst 0x00000013 # nop Opcode
      call wkomma
1:  drop

  .endif

  pushdaconst 0x00008067 | reg_tos << 7 # Opcode for jalr x8, x1, 0
  pop x1
  j wkomma

  .endif

# -----------------------------------------------------------------------------
  Definition Flag_visible, "dictionarystart"
dictionarystart: # Entry point for dictionary searches.
# -----------------------------------------------------------------------------

1:pushdatos
  laf x8, ThreadEnd            # In RAM:   Start with latest definition.
  lc x8, 0(x8)
  ret

  # Zwei Möglichkeiten: Vorwärtslink ist $FFFFFFFF --> Ende gefunden
  # Oder Vorwärtslink gesetzt, aber an der Stelle der Namenslänge liegt $FF. Dann ist das Ende auch da.
  # Diese Variante tritt auf, wenn nichts hinzugetan wurde, denn im festen Teil ist der Vorwärtslink
  # immer gesetzt und zeigt auf den Anfang des Schreib/Löschbaren Teils.

  # There are two possibilities to detect end of dictionary:
  # - Link is $FFFFFFFF
  # - Link is set, but points to memory that contains $FF in name length.
  # Last case happens if nothing is compiled yet, as the latest link in core always
  # points to the beginning of user writeable/eraseable part of dictionary space.

  # Dictionary entry structure:
  # 4 Bytes Link ( 4-aligned )
  # 4 Bytes Flags
  # 1 Byte  Name length
  #         Counted Name string and sometimes a padding zero to realign.
  #         Code.

# -----------------------------------------------------------------------------
  Definition Flag_visible, "dictionarynext" # ( address -- address flag )
dictionarynext: # Scans dictionary chain and returns true if end is reached.
# -----------------------------------------------------------------------------
  li x15, erasedcell # Check if link field is empty which does not happen with properly smudged flash definitions
  lc x14, 0(x8)
  beq x15, x14, true

    li x15, erasedbyte
    lbu x14, 2*CELL(x14) # Skip link and flags of the next definition to fetch its name length byte
    beq x15, x14, true

      lc x8, 0(x8)
        pushdaconst 0
        ret

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-find" # ( -- addr )
  CoreVariable hook_find
#------------------------------------------------------------------------------
  pushdaaddrf hook_find
  ret
  .varinit core_find  # No Pause defined for default

#------------------------------------------------------------------------------
  Definition Flag_visible, "find" # ( -- ? )
find:
#------------------------------------------------------------------------------
  laf x15, hook_find
  lc x15, 0(x15)
  .ifdef thejas32_pipeline_bug
  fence
  .endif
  .ifdef mipscore
  jr x15
  .else
  jalr zero, x15, 0
  .endif

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(find)"
core_find: # ( address length -- Code-Adresse Flags )
# -----------------------------------------------------------------------------
  push_x1_x10_x12

  # x15  Scratch
  # x10  Flags

  # x11  Destination Address
  # x12  Destination Flags

  # x8   Pointer for crawling through the dictionary

  li x11, 0  # No hits yet
  li x12, 0  # No hits have no Flags

  to_r_2 # String length and address

  call dictionarystart

1:# Loop through the dictionary

  lc x10, CELL(x8) # Fetch Flags to see if this definition is visible.
  li x15, Flag_invisible
  beq x15, x10, 2f # Skip this definition if invisible

  .ifdef erasedflashspecial
  li x15, erasedcell
  beq x15, x10, 2f # Skip this definition if invisible
  .endif

  # Definition is visible. Compare the name !
  dup  # ( x dict-addr ) tos=dict-addr
  addi x8, x8, 2*CELL # Skip Link and Flags
  call count          # Prepare an address-length string

  r_fetch_2
  call compare
  popda x15
  beq x15, zero, 2f

    # Found !
    # Skip name string
    dup
    addi x8, x8, 2*CELL # Skip Link and Flags
    call skipstring
    popda x11      # Note Code start address
    mv x12, x10    # Flags             Note Flags

    j 3f # RAM only, finished on first hit.

2:# Continue crawl.
  call dictionarynext
  popda x15 # ( x ) tos=dict-addr
  beq x15, zero, 1b

3:# Finished. Found something ?
  # Destination address <> 0 means successfully found.
  mv x8, x11    # Address = 0 means: Not found. Check for that !
  pushda x12    # Push Flags on Stack. ( Destination-Code Flags ) or ( 0 0 ).

  r_drop_2
  pop_x1_x10_x12
  ret
