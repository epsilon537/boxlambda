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

# Interrupt handling and CSR register access

  .include "../common/interrupt-common.s"
  .include "../common/cycles.s"

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mepc!" # Where did it occour ?
mepc_store:
# -----------------------------------------------------------------------------
  csrrw x0, mepc, x8
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mepc@" # Where did it occour ?
mepc_fetch:
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mepc, zero
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mtvec!" # Where did it occour ?
mtvec_store:
# -----------------------------------------------------------------------------
  csrrw x0, mtvec, x8
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mtvec@" # Where did it occour ?
mtvec_fetch:
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mtvec, zero
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mcause@" # Which interrupt ?
mcause_fetch:
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mcause, zero
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "unhandled" # Message for wild interrupts
unhandled:                             #   and handler for unused interrupts
# -----------------------------------------------------------------------------
  push x1
  write "Unhandled Interrupt mcause: "

  call mcause_fetch
  call hexdot

  write "mepc: "
  call mepc_fetch
  call hexdot

  writeln "!"

  pop x1
  ret

# -----------------------------------------------------------------------------
  .include "../common/irq-handler.s"
# -----------------------------------------------------------------------------

# Collection vector:
initinterrupt          collection, irq_collection, unhandled
#                      Forth-Name  Assembler-Name  Default handler
