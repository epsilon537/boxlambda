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

#.include "interrupt-femtorv.s"
.include "terminalhooks.s"
.include "uart_regs.s"

# -----------------------------------------------------------------------------
# Labels for a few hardware ports
# -----------------------------------------------------------------------------

.equ uart_txdata, (UART_BASE_ADDR + UART_TXDATA_ADDR)
.equ uart_rxdata, (UART_BASE_ADDR + UART_RXDATA_ADDR)
.equ uart_fifo, (UART_BASE_ADDR + UART_FIFO_ADDR)

# -----------------------------------------------------------------------------
uart_init:
# -----------------------------------------------------------------------------
  #la x15, irq_collection
  #csrrw zero, mtvec, x15  # MTVEC: Store address of exception handler
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "serial-emit"
serial_emit: # ( c -- ) Emit one character
# -----------------------------------------------------------------------------
  push x1

1:call serial_qemit
  popda x15
  beq x15, zero, 1b

  la x14, uart_txdata
  andi x8, x8, 0xFF
  sw x8, 0(x14)
  drop

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "serial-key"
serial_key: # ( -- c ) Receive one character
# -----------------------------------------------------------------------------
  push x1

1:call serial_qkey
  popda x15
  beq x15, zero, 1b

  pushdatos
  la x14, uart_rxdata
  lw x8, 0(x14)
  andi x8, x8, 0xFF

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "serial-emit?"
serial_qemit:  # ( -- ? ) Ready to send a character ?
# -----------------------------------------------------------------------------
  push x1
  call pause

  pushdatos
  li x8, uart_fifo
  lw x8, 0(x8)
  li x14, UART_FIFO_TX_Z_MASK
  and x8, x8, x14 #Space available bit

  sltiu x8, x8, 1 # 0<>
  addi x8, x8, -1

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "serial-key?"
serial_qkey:  # ( -- ? ) Is there a key press ?
# -----------------------------------------------------------------------------
  push x1
  call pause

  pushdatos
  li x8, uart_fifo
  lw x8, 0(x8)
  andi x8, x8, UART_FIFO_RX_Z_MASK #Data avl in Rx FIFO

  sltiu x8, x8, 1 # 0<>
  addi x8, x8, -1

  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "reset"
# -----------------------------------------------------------------------------
  csrrci zero, mstatus, 8 # Clear Machine Interrupt Enable Bit
  call Reset
  j quit

# -----------------------------------------------------------------------------
  Definition Flag_visible, "bye"
# -----------------------------------------------------------------------------
  # Restore stack to where it was at forth_repl entry.
  laf x15, returnstackstart

  # Set data stack
  mv a0, x8
  mv a1, x9

  lc sp, 0(x15)
  # Restore impacted risc-v calling convention registers that were saved
  # at forth_reply entry
  pop x9
  pop x8
  pop tp
  pop gp
  pop ra # We're going to return to the forth_repl caller
  ret

