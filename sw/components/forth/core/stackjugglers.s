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
#

# Stack jugglers

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
dup_jump:
# -----------------------------------------------------------------------------
  dup
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_1|Flag_inline|Flag_noframe, "drop" # ( x -- )
drop_jump:
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
over_jump:
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
  Definition Flag_visible, "depth"
                                  # ( -- Number of elements that have been on datastack before )
# -----------------------------------------------------------------------------
  # Calculate the stack level
  laf x15, __datastack # Calculate stack fill gauge
  sub x15, x15, x9
  pushdatos
  srai x8, x15, CELLSHIFT # Divide through bytes/cell.
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "rdepth"
# -----------------------------------------------------------------------------
  laf x15, gp_tp_sp
  lc x15, 8(x15) # Calculate stack fill gauge
  sub x15, x15, sp
  pushdatos
  srai x8, x15, CELLSHIFT # Divide through bytes/cell.
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, ">r" # Puts the top element of the data stack onto the return stack.
#------------------------------------------------------------------------------
  push x8
  drop
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "r>" # Retrieve the cached element from the return stack
#------------------------------------------------------------------------------
  pushdatos
  pop x8
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "r@" # Copies the top element of the return stack to the data stack
#------------------------------------------------------------------------------
  pushdatos
  lc x8, 0(sp)
  ret

#------------------------------------------------------------------------------
  Definition Flag_inline, "rdrop" # Removes the top element of the return stack
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

