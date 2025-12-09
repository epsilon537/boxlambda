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

# Interrupt tools for RISC-V

# -----------------------------------------------------------------------------
  Definition Flag_visible, "eint?" # ( -- ) Are Interrupts enabled ?
eintq:
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mstatus, zero
  andi x8, x8, 8 # Machine Interrupt Enable
  sltiu x8, x8, 1
  addi x8, x8, -1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe, "eint"
# -----------------------------------------------------------------------------
  csrrsi zero, mstatus, 8 # Set Machine Interrupt Enable Bit
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe, "dint"
# -----------------------------------------------------------------------------
  csrrci zero, mstatus, 8 # Clear Machine Interrupt Enable Bit
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline|Flag_noframe, "wfi"
# -----------------------------------------------------------------------------
  wfi
  ret

