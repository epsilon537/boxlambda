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

# Initializes pointers and variables for dictionary upon start-up.
# This runs one time after start-up, no registers are saved here.


   # Hardware sets return stack pointer on startup from vector table.
   # Set Return stack pointer here (again) just in case this might be a software re-entry.

   # TOS magic number to see spurious stack underflows in .s

  # -----------------------------------------------------------------------------
  # Initialize essential registers

  # RS/sp is already set up at this point
  li  x8, 42          # TOS
  laf x9, __datastack # PSP

  # -----------------------------------------------------------------------------
  # x16-x31: Unused.
  # -----------------------------------------------------------------------------

  # Short: Search for begin and end in Flash.

   # Set dictionary pointer into RAM first
   laf x14, Dictionarypointer
   laf x15, RamDictionaryStart
   sc x15, 0(x14)

   # Point ThreadEnd to start of CoreDictionary (.forth_core section in IMEM)
   laf x14, ThreadEnd
   la x15, CoreDictionaryStart
   sc x15, 0(x14)

 #  write "Setze ThreadEnd:"
 #  pushdaconst ThreadEnd
 #  lc x8, 0(x8)
 #  # laf x8, CoreDictionaryStart
 #  call hexdot
 #  writeln ""

  # Register allocation here:

  #    x10 Temporary this and that
  #    x11 Current Flags
  #    x13 Temporary this and that
  #    x7  Keeps track of allocated RAM
  # TOS=x8 Pointer that crawls through dictionary

  pushdatos
  la x8,  CoreDictionaryStart  # Start from here
  laf x7, RamDictionaryEnd     # Variables start at the end of RAM dictionary

ScanCoreWords:
#  pushda x8
#  call hexdot
#  writeln "Hangelschleife"
  lc x11, CELL(x8)  # Fetch current Flags

  li x13, Flag_invisible # Flag_invisible ? Skip invisible definitions
  beq x11, x13, ScanCoreWords_MemAlloc_complete

  .ifdef erasedflashspecial
  li x13, erasedword # Skip empty flash locations
  beq x11, x13, ScanCoreWords_MemAlloc_complete
  .endif

    # This definition is visible. Check if it allocates RAM.

    andi x13, x11, Flag_buffer & ~Flag_visible
    beq x13, zero, 1f # No buffer requested.

      # Search for end of code of current definition.
      push x8

      addi x8, x8, 2*CELL
      call skipstring  # x8 points to start of code of current definition
      call findendofdefinition # Advance pointer to end of code. This is detected by "bx lr" or "pop {pc}" opcodes.

      # x8 now points to the location of the initialisation data at the end of code of current definition.

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

      # Subtract from the allocated RAM pointer (the allocated RAM pointer advances downwards).
      sub x7, x7, x13
      pop x8
      j ScanCoreWords_MemAlloc_complete # Finished

checksums: # Self-test register checksums for x1 - x15.
      .word             0x0000012F, 0x20000000, 0x24200133  #  x1 - x3   No checksum necessary for hardwired zero register x0
      .word 0x03244425, 0x79901867, 0x00000000, 0x09313607  #  x4 - x7
      .word 0x2442A08A, 0x03B031A0, 0x83942086, 0x00060001  #  x8 - x11
      .word 0x18000008, 0x272E0A20, 0x28602D5F, 0x3C292F5C  # x12 - x15

1:  andi x13, x11, Flag_ramallot & ~Flag_visible
    beq x13, zero, ScanCoreWords_MemAlloc_complete # Does not allocate RAM
      # This one allocates RAM, Flags are not needed anymore.

      andi x11, x11, 0x0F # Das unterste Nibble maskieren  Mask lower 4 bits that contains amount of cells requested.

 #     pushda x11
 #     call hexdot
 #     writeln " initram"

        # Zero byte requests are handled by move itself, no need to catch this special case. Sounds strange, but is useful to have two handles for one variable.
        slli x11, x11, CELLSHIFT # Multiply by size of cell in bytes
        sub x7, x7, x11 # Subtract from the allocated RAM pointer (the allocated RAM pointer advances downwards).

        # Initialise the freshly allocated RAM locations !

        # Search for end of code of current definition.
        pushdatos
        addi x8, x8, 2*CELL
        call skipstring  # x8 points to start of code of current definition
        call findendofdefinition # Advance pointer to end of code. This is detected by "bx lr" or "pop {pc}" opcodes.

        # x8 now points to the location of the initialisation at the end of code of current definition.
        # Copy desired amount of r1 bytes from [r0] to [r5].
        # Source
        pushdadouble x7, x11
        #pushda x7 # Target
        #pushda x11 # Bytes  Amount
        call move

ScanCoreWords_MemAlloc_complete:
  # Finished RAM allocation and initialisation.

  # Continue crawl.
  call dictionarynext
  popda x10
  beq x10, zero, ScanCoreWords

  laf x10, SecondThreadEnd
  sc x8, 0(x10) # Set SecondThreadEnd pointer to latest core word i.e. link SecondThread to Thread.
  drop

  laf x10, VariablesPointer # Set variables pointer to RAM allocations made by core words.
  sc x7, 0(x10)


  # writeln "Loop completed"

  laf x10, __forth_imem_start
  laf x11, SecondDictionaryPointer # Set SecondDictionaryPointer to IMEM region.
  sc x10, 0(x11)

  .ifdef initflash
  call initflash
  .endif
