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

# Speicherzugriffe aller Art
# Memory access

# -----------------------------------------------------------------------------
  Definition Flag_visible, "move"  # Move some bytes around. This can cope with overlapping memory areas.
move:  # ( Quelladdr Zieladdr Byteanzahl -- ) ( Source Destination Count -- )
# -----------------------------------------------------------------------------

  push x10

  popdadouble x14, x15
  # x8: Source address
  # x15: Destination address
  # x14: Count

  # push x1
  # push x15
  # push x14
  # write " Move: "
  # pushda x8
  # call hexdot
  # pushda x15
  # call hexdot
  # pushda x14
  # call hexdot
  # writeln ""
  # pop x14
  # pop x15
  # pop x1

  beq  x14, zero, 3f  # Nothing to do if count is zero.
  beq  x8,  x15,  3f  # If source and destination are the same, nothing to do, too.
  bltu x15,  x8,  2f  # Compare destination with source address to find out which direction to copy.

  # Source < Destination --> Backward move

  add x8, x8, x14
  add x15, x15, x14

1:lbu x10, -1(x8)
  sb  x10, -1(x15)

  addi x8, x8, -1
  addi x15, x15, -1

  addi x14, x14, -1
  bne x14, zero, 1b

  j 3f

  # Source > Destination --> Forward move

2:lbu x10, 0(x8)
  sb  x10, 0(x15)

  addi x8, x8, 1
  addi x15, x15, 1

  addi x14, x14, -1
  bne x14, zero, 2b

3:drop
  pop x10
  ret

# : s1  s" 0123456789ABCDEF" 2dup 2>r   drop    4 + dup 5 + 3 move  2r> ." >" type ." <" ; \ >012345678456CDEF< ok.
# : s2  s" 0123456789ABCDEF" 2dup 2>r   drop    4 + dup 2 - 3 move  2r> ." >" type ." <" ; \ >0145656789ABCDEF< ok.

# -----------------------------------------------------------------------------
  Definition Flag_visible, "fill"  # Fill memory with given byte.
fill:  # ( Destination Count Filling -- )
# -----------------------------------------------------------------------------
  # 6.1.1540 FILL CORE ( c-addr u char -- ) If u is greater than zero, store char in each of u consecutive characters of memory beginning at c-addr.

  popdadouble x14, x15
  # x14: Filling byte
  # x15: Count
  # x8:  Destination

  beq x15, zero, 2f

1:sb x14, 0(x8)
  addi x8, x8, 1
  addi x15, x15, -1
  bne x15, zero, 1b

2:drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "@" # ( 32-addr -- x )
                              # Loads the cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x8C000000 | reg_tos << 21 | reg_tos << 16
  .else
  .ifdef RV64
  li x15, 0x00003003 | reg_tos << 7  | reg_tos << 15  # ld x8, 0(x8)
  .else
  li x15, 0x00002003 | reg_tos << 7  | reg_tos << 15  # lw x8, 0(x8)
  .endif
  .endif

# -----------------------------------------------------------------------------
opcodiere_fetch_adresskonstante:
# -----------------------------------------------------------------------------
  push_x1_x10

  mv x10, x15
  #  x8: Einzufügende Adresse
  # x10: Opcode

  # Konstanten aufräumen

  to_r # Hole die oberste Konstante
  addi x13, x13, -1 # Ziehe die oberste von der Zahl an Faltkonstanten ab
  call konstantenschreiben # und schreibe alle anderen Konstanten
  r_from # Lege die oberste Konstante zurück, um sie später zu bearbeiten

  # Vorspann schreiben

  call dup_komma

  # Probe, ob sich die Konstante nicht auch kürzer laden lässt:

  .ifdef mipscore
  li x15, 0xFFFF8000
  .else
  li x15, -2048 # 0xFFFFF800
  .endif
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, 2f

1:# Kurze Variante mit nur einem Opcode: Register x8 durch x0 austauschen.
  .ifdef mipscore
  li x15, ~(0x1F << 21)
  .else
  li x15, ~0x000F8000
  .endif
  and x10, x10, x15
  j 4f

2:# Lange Variante mit zwei Opcodes.

  .ifdef RV64

  # Probe ob die Konstante über die Zwei-Opcode-Grenzen hinausragt:

  li x15, 0xFFFFFFFF80000000
  and x14, x8, x15
  beq x14, x15, 1f
  beq x14, zero, 1f

    pushdaconst reg_tos
    call registerliteralkomma

    pushda x10
    pop_x1_x10
    j wkomma
1:

  .endif

  # Korrektur fürs negative Vorzeichen bei der Addition
  .ifdef mipscore
  li x15, 0x8000
  .else
  li x15, 0x800
  .endif
  and x15, x15, x8
  beq x15, zero, 3f
    .ifdef mipscore
    li x15, 0x10000
    .else
    li x15, 0x1000
    .endif
    add x8, x8, x15
3:

  dup
  .ifdef mipscore
  srli x8, x8, 16
  li x15, 0x3C000000 | reg_tos << 16
  or x8, x8, x15
  .else
  li x15, -4096 # 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x000000037 | reg_tos << 7  # lui x8, ...
  .endif
  call wkomma


4:# Nur ein Opcode, oder der zweite Teil bei zwei Opcodes

  .ifdef mipscore
  li x15, 0xFFFF
  and x15, x8, x15
  .else
  li x15, 0xFFF  # Imm [11:0]
  and x15, x8, x15
  sll x15, x15, 20
  .endif

  or x8, x10, x15
  pop_x1_x10
  j wkomma

#    .ifdef mipscore # NOP for load delay slot ***
#    pushdaconst 0
#    call wkomma
#   .endif

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "!" # ( x 32-addr -- )
# Given a value 'x' and a cell-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
store:
  lc x15, 0(x9)
  sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x14, 0xAC000000 | reg_tos  << 16 | reg_tmp2 << 21 # Eine Konstante
  li x15, 0xAC000000 | reg_tmp1 << 16 | reg_tmp2 << 21 # Zwei Konstanten
  .else
  .ifdef RV64
  li x14, 0x00003023 | reg_tos  << 20 | reg_tmp2 << 15 # sd x8,  0(x14)
  li x15, 0x00003023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sd x15, 0(x14)
  .else
  li x14, 0x00002023 | reg_tos  << 20 | reg_tmp2 << 15 # sw x8,  0(x14)
  li x15, 0x00002023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sw x15, 0(x14)
  .endif
  .endif

  # Fallthrough:

# -----------------------------------------------------------------------------
opcodiere_store_adresskonstante:
# -----------------------------------------------------------------------------

  push_x1_x10

  addi x13, x13, -1
  bne x13, zero, 1f

  # Eine Konstante
  mv x10, x14
  call opcodiere_store_adresskonstante_intern
  call dropkomma
  j 2f

1:# Zwei Konstanten
  mv x10, x15

  swap
  pushdaconst reg_tmp1
  call registerliteralkomma

  call opcodiere_store_adresskonstante_intern

2:pop_x1_x10
  ret


opcodiere_store_adresskonstante_intern:

  push x1 # x10 ist schon gesichert, wenn wir hier ankommen.

  #  x8: Einzufügende Adresse
  # x10: Opcode

  # Probe, ob sich die Konstante nicht auch kürzer laden lässt:

  .ifdef mipscore
  li x15, 0xFFFF8000
  .else
  li x15, -2048 # 0xFFFFF800
  .endif
  and x14, x8, x15
  beq x14, x15, 1f
  bne x14, zero, 2f

1:# Kurze Variante mit nur einem Opcode: Register x14 durch x0 austauschen.
  .ifdef mipscore
  li x15, ~(0x1F << 21)
  .else
  li x15, ~0x000F8000
  .endif
  and x10, x10, x15
  j 4f

2:# Lange Variante mit zwei Opcodes.

  .ifdef RV64

  # Probe ob die Konstante über die Zwei-Opcode-Grenzen hinausragt:

  li x15, 0xFFFFFFFF80000000
  and x14, x8, x15
  beq x14, x15, 1f
  beq x14, zero, 1f

    pushdaconst reg_tmp2
    call registerliteralkomma

    pushda x10
    pop x1
    j wkomma
1:

  .endif


  # Korrektur fürs negative Vorzeichen bei der Addition
  .ifdef mipscore
  li x15, 0x8000
  .else
  li x15, 0x800
  .endif
  and x15, x15, x8
  beq x15, zero, 3f
    .ifdef mipscore
    li x15, 0x10000
    .else
    li x15, 0x1000
    .endif
    add x8, x8, x15
3:

  dup
  .ifdef mipscore
  srli x8, x8, 16
  li x15, 0x3C000000 | reg_tmp2 << 16
  or x8, x8, x15
  .else
  li x15, -4096 # 0xFFFFF000
  and x8, x8, x15
  ori  x8, x8, 0x000000037 | reg_tmp2 << 7  # lui x14, ...
  .endif
  call wkomma


4:# Nur ein Opcode, oder der zweite Teil bei zwei Opcodes


  .ifdef mipscore
  li x15, 0xFFFF
  and x15, x8, x15
  .else
  andi x15, x8, 0x1F  # Imm [4:0]
  sll x15, x15, 7
  or x10, x10, x15

  li x15, 0xFE0  # Imm [11:5]
  and x15, x8, x15
  srl x15, x15, 5
  sll x15, x15, 25
  .endif

  or x8, x10, x15

  pop x1
  j wkomma

# -----------------------------------------------------------------------------
  Definition Flag_visible, "+!" # ( x 32-addr -- )
                               # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lc x14, 0(x8)
    add x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

.ifdef RV64

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "w@" # ( 32-addr -- x )
                              # Loads the word at 'addr'.
# -----------------------------------------------------------------------------
  lwu x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x00006003 | reg_tos << 7  | reg_tos << 15  # lwu x8, 0(x8)

  j opcodiere_fetch_adresskonstante

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "w@signed" # ( 32-addr -- x )
                              # Loads the word at 'addr'.
# -----------------------------------------------------------------------------
  lw x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x15, 0x00002003 | reg_tos << 7  | reg_tos << 15  # lw x8, 0(x8)

  j opcodiere_fetch_adresskonstante

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "w!" # ( x 32-addr -- )
# Given a value 'x' and an 32-bit-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
wstore:
  lc x15, 0(x9)
  sw x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  li x14, 0x00002023 | reg_tos  << 20 | reg_tmp2 << 15 # sw x8,  0(x14)
  li x15, 0x00002023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sw x15, 0(x14)

  j opcodiere_store_adresskonstante


# -----------------------------------------------------------------------------
  Definition Flag_visible, "w+!" # ( x 32-addr -- )
                                # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lwu x14, 0(x8)
    add x15, x15, x14
    sw x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

.endif


# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "h@" # ( 16-addr -- x )
                              # Loads the half-word at 'addr'.
# -----------------------------------------------------------------------------
  lhu x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x94000000 | reg_tos << 21 | reg_tos << 16
  .else
  li x15, 0x00005003 | reg_tos << 7  | reg_tos << 15  # lhu x8, 0(x8)
  .endif
  j opcodiere_fetch_adresskonstante

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "h@signed" # ( 16-addr -- x )
                              # Loads the half-word at 'addr'.
# -----------------------------------------------------------------------------
  lh x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x84000000 | reg_tos << 21 | reg_tos << 16
  .else
  li x15, 0x00001003 | reg_tos << 7  | reg_tos << 15  # lh x8, 0(x8)
  .endif
  j opcodiere_fetch_adresskonstante

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "h!" # ( x 16-addr -- )
# Given a value 'x' and an 16-bit-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
hstore:
  lc x15, 0(x9)
  sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x14, 0xA4000000 | reg_tos  << 16 | reg_tmp2 << 21 # Eine Konstante
  li x15, 0xA4000000 | reg_tmp1 << 16 | reg_tmp2 << 21 # Zwei Konstanten
  .else
  li x14, 0x00001023 | reg_tos  << 20 | reg_tmp2 << 15 # sh x8,  0(x14)
  li x15, 0x00001023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sh x15, 0(x14)
  .endif

  j opcodiere_store_adresskonstante


# -----------------------------------------------------------------------------
  Definition Flag_visible, "h+!" # ( x 16-addr -- )
                                # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lhu x14, 0(x8)
    add x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "c@" # ( 8-addr -- x )
                              # Loads the byte at 'addr'.
# -----------------------------------------------------------------------------
  lbu x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x90000000 | reg_tos << 21 | reg_tos << 16
  .else
  li x15, 0x00004003 | reg_tos << 7  | reg_tos << 15  # lbu x8, 0(x8)
  .endif
  j opcodiere_fetch_adresskonstante

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "c@signed" # ( 8-addr -- x )
                              # Loads the byte at 'addr'.
# -----------------------------------------------------------------------------
  lb x8, 0(x8)
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x15, 0x80000000 | reg_tos << 21 | reg_tos << 16
  .else
  li x15, 0x00000003 | reg_tos << 7  | reg_tos << 15  # lb x8, 0(x8)
  .endif
  j opcodiere_fetch_adresskonstante

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe|Flag_opcodable, "c!" # ( x 8-addr -- )
# Given a value 'x' and an 8-bit-aligned address 'addr', stores 'x' to memory at 'addr', consuming both.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  sb x15, 0(x8)
  lc x8, CELL(x9)
  addi x9, x9, 2*CELL
  ret

  # ---------------------------------------------
  #  Opcodier-Einsprung:

  .ifdef mipscore
  li x14, 0xA0000000 | reg_tos  << 16 | reg_tmp2 << 21 # Eine Konstante
  li x15, 0xA0000000 | reg_tmp1 << 16 | reg_tmp2 << 21 # Zwei Konstanten
  .else
  li x14, 0x00000023 | reg_tos  << 20 | reg_tmp2 << 15 # sb x8,  0(x14)
  li x15, 0x00000023 | reg_tmp1 << 20 | reg_tmp2 << 15 # sb x15, 0(x14)
  .endif

  j opcodiere_store_adresskonstante


# -----------------------------------------------------------------------------
  Definition Flag_visible, "c+!" # ( x 8-addr -- )
                               # Adds 'x' to the memory cell at 'addr'.
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lbu x14, 0(x8)
    add x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret



# -----------------------------------------------------------------------------
  Definition Flag_visible, "bis!" # ( x 32-addr -- )  Set bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lc x14, 0(x8)
    or x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bic!" # ( x 32-addr -- )  Clear bits
  # Löscht die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lc x14, 0(x8)
    and x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "xor!" # ( x 32-addr -- )  Toggle bits
  # Wechselt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lc x14, 0(x8)
    xor x15, x15, x14
    sc x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bit@" # ( x 32-addr -- Flag )  Check bits
  # Prüft, ob Bits in der Speicherstelle gesetzt sind
# -----------------------------------------------------------------------------
  lc x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret

.ifdef RV64


# -----------------------------------------------------------------------------
  Definition Flag_visible, "wbis!" # ( x 32-addr -- )  Set bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lwu x14, 0(x8)
    or x15, x15, x14
    sw x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "wbic!" # ( x 32-addr -- )  Clear bits
  # Löscht die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lwu x14, 0(x8)
    and x15, x15, x14
    sw x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "wxor!" # ( x 32-addr -- )  Toggle bits
  # Wechselt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lwu x14, 0(x8)
    xor x15, x15, x14
    sw x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "wbit@" # ( x 32-addr -- Flag )  Check bits
  # Prüft, ob Bits in der Speicherstelle gesetzt sind
# -----------------------------------------------------------------------------
  lwu x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret

.endif

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hbis!" # ( x 16-addr -- )  Set bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lhu x14, 0(x8)
    or x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hbic!" # ( x 16-addr -- )  Clear bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lhu x14, 0(x8)
    and x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hxor!" # ( x 16-addr -- )  Toggle bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lhu x14, 0(x8)
    xor x15, x15, x14
    sh x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hbit@" # ( x 16-addr -- Flag )  Check bits
  # Prüft, ob Bits in der Speicherstelle gesetzt sind
# -----------------------------------------------------------------------------
  lhu x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret


# -----------------------------------------------------------------------------
  Definition Flag_visible, "cbis!" # ( x 8-addr -- )  Set bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lbu x14, 0(x8)
    or x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cbic!" # ( x 8-addr -- )  Clear bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  inv x15
    lbu x14, 0(x8)
    and x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cxor!" # ( x 8-addr -- )  Toggle bits
  # Setzt die Bits in der Speicherstelle
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
    lbu x14, 0(x8)
    xor x15, x15, x14
    sb x15, 0(x8)
  lc x8, 1*CELL(x9)
  addi x9, x9, 2*CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cbit@" # ( x 8-addr -- Flag )  Check bits
  # Prüft, ob Bits in der Speicherstelle gesetzt sind
# -----------------------------------------------------------------------------
  lbu x15, 0(x8)
  drop
  and x8, x8, x15
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret
