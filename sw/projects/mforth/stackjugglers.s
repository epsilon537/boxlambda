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

# Stackjongleure
# Stack jugglers

# Stack pointers

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe, "sp@" # ( -- a-addr )
# -----------------------------------------------------------------------------
  pushdatos
  mv x8, x9
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe, "sp!" # ( a-addr -- )
# -----------------------------------------------------------------------------
  mv x9, x8
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline, "rp@" # ( -- a-addr )
# -----------------------------------------------------------------------------
  pushdatos
  mv x8, sp
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline, "rp!" # ( a-addr -- )
# -----------------------------------------------------------------------------
  mv sp, x8
  drop
  ret

# Stack juggling

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "dup" # ( x -- x x )
dup_einsprung:
# -----------------------------------------------------------------------------
  dup
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "drop" # ( x -- )
drop_einsprung:
# -----------------------------------------------------------------------------
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "?dup" # ( x -- 0 | x x )
# -----------------------------------------------------------------------------
  beq x8, zero, 1f
    pushdatos
1:ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "swap" # ( x y -- y x )
# -----------------------------------------------------------------------------
  swap
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "nip" # ( x y -- x )
# -----------------------------------------------------------------------------
  nip
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "over" # ( x y -- x y x )
# -----------------------------------------------------------------------------
over_einsprung:
  pushdatos
  lc x8, CELL(x9)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "tuck" # ( x1 x2 -- x2 x1 x2 )
# -----------------------------------------------------------------------------
tuck:
  lc x15,   0(x9)
  addi x9, x9, -CELL
  sc x8, CELL(x9)
  sc x15,   0(x9)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_3, "rot" # ( x w y -- w y x )
# -----------------------------------------------------------------------------
rot:
  lc x15,    0(x9)
  lc x14, CELL(x9)
  sc x8,     0(x9)
  sc x15, CELL(x9)
  mv x8, x14
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_3, "-rot" # ( x w y -- y x w )
# -----------------------------------------------------------------------------
minusrot:
  lc x15,    0(x9)
  lc x14, CELL(x9)
  sc x14,    0(x9)
  sc x8,  CELL(x9)
  mv x8, x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe, "pick" # ( xu .. x1 x0 u -- xu ... x1 x0 xu )
# -----------------------------------------------------------------------------
pick:
  slli x8, x8, CELLSHIFT
  add x8, x8, x9
  lc x8, 0(x8)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "depth" # ( -- Zahl der Elemente, die vorher auf den Datenstack waren )
                                  # ( -- Number of elements that have been on datastack before )
# -----------------------------------------------------------------------------
  # Berechne den Stackfüllstand
  laf x15, __datastack # Anfang laden  Calculate stack fill gauge
  sub x15, x15, x9          # und aktuellen Stackpointer abziehen
  pushdatos
  srai x8, x15, CELLSHIFT # Divide through bytes/cell.
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "rdepth"
# -----------------------------------------------------------------------------
  # Berechne den Stackfüllstand
  laf x15, returnstackstart
  lc x15, 0(x15) # Anfang laden  Calculate stack fill gauge
  sub x15, x15, sp          # und aktuellen Stackpointer abziehen
  pushdatos
  srai x8, x15, CELLSHIFT # Divide through bytes/cell.
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, ">r" # Legt das oberste Element des Datenstacks auf den Returnstack.
#------------------------------------------------------------------------------
  push x8
  drop
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "r>" # Holt das zwischengespeicherte Element aus dem Returnstack zurück
#------------------------------------------------------------------------------
  pushdatos
  pop x8
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "r@" # Kopiert das oberste Element des Returnstacks auf den Datenstack
#------------------------------------------------------------------------------
  pushdatos
  lc x8, 0(sp)
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "rdrop" # Entfernt das oberste Element des Returnstacks
#------------------------------------------------------------------------------
  addi sp, sp, CELL
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline, "rpick" # ( u -- xu R: xu .. x1 x0 -- xu ... x1 x0 )
# -----------------------------------------------------------------------------
  slli x8, x8, CELLSHIFT
  add x8, x8, sp
  lc x8, 0(x8)
  ret
