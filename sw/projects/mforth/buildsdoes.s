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

# -----------------------------------------------------------------------------
insert_jalrx8: # ( Ziel Opcodelücke -- )
# -----------------------------------------------------------------------------
  push_x1_x10

  popda x10 # Opcodelücke

  dup
  # High-Part
  srli x8, x8, 16
  li x15, 0x3C000000 | reg_tmp1 << 16 # lui x15, 0
  or x8, x8, x15
  pushda x10
  call kommasomewhere

  # Low-Part
  andi x8, x8, 0xFFFF
  li x15, 0x34000000 | reg_tmp1 << 21 | reg_tmp1 << 16 # ori x15, x15, 0
  or x8, x8, x15
  pushda x10
  addi x8, x8, 4
  pop_x1_x10
  j kommasomewhere

.else

.ifndef RV64
# -----------------------------------------------------------------------------
insert_jalrx8: # ( Ziel Opcodelücke -- )
# -----------------------------------------------------------------------------
  push_x1_x10

  popda x10 # Opcodelücke

  # Korrektur fürs negative Vorzeichen
  li x15, 0x800
  and x15, x15, x8
  beq x15, zero, 1f
    li x15, 0x00001000
    add x8, x8, x15
1:

  dup
  li x15, 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x00000037 | reg_tmp1 << 7 # lui x15, ...

  pushda x10
  call kommasomewhere

  slli x8, x8, 20
  li x15, 0x00000067 | reg_tos << 7 | reg_tmp1 << 15 # jalr x8, x15, 0
  or x8, x8, x15

  pushda x10
  addi x8, x8, 4
  pop_x1_x10
  j kommasomewhere

.endif
.endif

# -----------------------------------------------------------------------------
  Definition Flag_inline, "does>"
does: # Gives freshly defined word a special action.
      # Has to be used together with <builds !
# -----------------------------------------------------------------------------
    # At the place where does> is used, a jump to dodoes is inserted and
    # after that a R> to put the address of the definition entering the does>-part
    # on datastack. This is a very special implementation !

  pushdatos
  # Den Aufruf mit absoluter Adresse einkompilieren. Perform this call with absolute addressing.
  .ifdef mipscore
    la x15, dodoes
    jalr x8, x15
  .else
    .ifdef RV64
      ld x15, dodoes_addr
      j does_jalr
             .p2align 3
dodoes_addr: .dword dodoes
does_jalr:
      jalr x8, x15, 0
    .else
      lui x15, %hi(dodoes)
      jalr x8, x15, %lo(dodoes)
    .endif
  .endif

  ret # Very important as delimiter as does> itself is inline.

dodoes:
  # On the stack: ( Address-to-call R: Return-of-dodoes-itself )
  # Now insert a call into the latest definition which was hopefully prepared by <builds

  # Save and change dictionary pointer to insert a call sequence:

  push x1

  pushdaaddrf Entrypoint
  lc x8, 0(x8)

  .ifdef compressed_isa
    addi x8, x8, 8    # Skip pop x1 and pushdatos opcodes
    andi x15, x8, 2   # Align on 4
    add x8, x8, x15
  .else
    addi x8, x8, 16   # Skip pop x1 and pushdatos opcodes
  .endif

  .ifdef RV64
    andi x15, x8, 4   # Align on 8
    add x8, x8, x15

    addi x8, x8, 8    # Skip auipc & j opcodes
  .endif

  .ifdef flash8bytesblockwrite
    dup
    call addrinflash
    popda x15
    beq x15, zero, 1f

      andi x15, x8, 7
      beq x15, zero, 1f

        addi x8, x8, 4
1:
  .endif

  # ( Ziel Opcodelücke )

  .ifdef RV64
    over
    addiw x8, x8, 0
    over
    call kommasomewhere
    swap
    srli x8, x8, 32
    swap
    addi x8, x8, 4
    call kommasomewhere
  .else
    call insert_jalrx8
  .endif

  call smudge

  addi sp, sp, CELL # Skip one return layer
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "<builds"
builds: # Brother of does> that creates a new definition and leaves space to insert a call instruction later.
#------------------------------------------------------------------------------
  push x1
  call create # Create new empty definition
  call push_x1_komma # Write opcodes for push x1

  call dup_komma

  .ifdef compressed_isa
    call align4komma
  .endif

  .ifdef flash8bytesblockwrite

    call compiletoramq
    popda x15
    bne x15, zero, 1f

      call here
      andi x15, x8, 7
      drop
      beq x15, zero, 1f # Address and 7 will be either 0 or 4 and shall be aligned on 8.

      .ifdef mipscore
        pushdaconst 0x00000025 # nop
      .else
        pushdaconst 0x00000013 # nop
      .endif
      call wkomma

1:
  .endif

  .ifdef RV64
    call align8komma

    # Die Konstante darf nicht das Letzte in der Definition sein,
    # weil in den 64 Bits oben auch 4 leere Bytes sein können und in dem
    # Falle sonst später in smudge ein NOP als "Flashenderkennungsschutz"
    # angefügt werden würde, was das Konstrukt zerstört.

    # Deshalb springe ich hier über die Konstante drüber.

    pushdaconst 0x00000017 | reg_tmp1 << 7 # auipc x15, 0
    call wkomma

    pushdaconst 0x00c0006f # j über die inline folgende 8-Byte-Konstante
    call wkomma
  .endif

  # Die Lücke passt sowohl für das 32-Bit lui/jalr-Paar als auch für die 64-Bit Konstante.
  pushdaconst 2*4 # A call instruction or its preparation will go here - but I don't know its target address for now.
  call allot # Make a hole to insert the destination later.

  .ifdef RV64
    pushdaconst 0x00803003 | reg_tmp1 << 7 | reg_tmp1 << 15 # ld x15, 8(x15)
    call wkomma

    pushdaconst 0x00000067 | reg_tos  << 7 | reg_tmp1 << 15 # jalr x8, x15, 0
    call wkomma
  .endif

  .ifdef mipscore
    pushdaconst 0x00000009 | reg_tos << 11 | reg_tmp1 << 21 # jalr x8, x15
    call wkomma

    pushdaconst 0x00000025 # nop = or zero, zero, zero  Wichtig, damit keine Flash-Ende-Füllung angefügt wird.
    call wkomma
  .endif

  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "create" # ANS-Create with default action.
#------------------------------------------------------------------------------
  push x1
  call builds
  # Copy of the inline-code of does>

  pushdatos
  # Den Aufruf mit absoluter Adresse einkompilieren. Perform this call with absolute addressing.
  .ifdef mipscore
    la x15, dodoes
    jalr x8, x15
  .else
    .ifdef RV64
      ld x15, dodoes_addr_create
      j does_jalr_create
                    .p2align 3
dodoes_addr_create: .dword dodoes
does_jalr_create:
      jalr x8, x15, 0
    .else
      lui x15, %hi(dodoes)
      jalr x8, x15, %lo(dodoes)
    .endif
  .endif

  pop x1
  ret
