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

# Cycle counters

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cycles64" # Uptime in cycles, 64 bits
cycles64:
# -----------------------------------------------------------------------------
  pushdatos

1:rdcycleh x15
  rdcycle  x8
  rdcycleh x14
  bne x15, x14, 1b

  pushda x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cycles" # Uptime in cycles, 32 bits
cycles:
# -----------------------------------------------------------------------------
  pushdatos
  rdcycle  x8
  ret
