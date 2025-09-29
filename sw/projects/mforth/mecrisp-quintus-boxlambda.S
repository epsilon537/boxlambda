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

# -----------------------------------------------------------------------------
# Swiches for capabilities of this chip
# -----------------------------------------------------------------------------

.option norelax
.section .forth_core

# -----------------------------------------------------------------------------
# Include the Forth core of Mecrisp-Quintus
# -----------------------------------------------------------------------------
.include "forth-core.s"

# -----------------------------------------------------------------------------
.global StartmForth
StartmForth: # Forth begins here
Reset:
# -----------------------------------------------------------------------------

  .include "catchmempointers.s"

  call uart_init

  welcome " for RISC-V RV32IM by Matthias Koch." "BoxLambda port by Ruben Lysens."

  # Ready to fly :!
  .include "boot.s"

#.org 0x20000, 0xFF
