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

  .include "interrupt-common.s"

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mcause" # Which interrupt ?
interrupt_cause:
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mcause, zero
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mepc" # Where did it occour ?
interrupt_location:
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mepc, zero
  ret


# -----------------------------------------------------------------------------
  Definition Flag_visible, "mie" # Machine Interrupt Enable
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mie, zero
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mip" # Machine Interrupt Pending
# -----------------------------------------------------------------------------
  pushdatos
  csrrs x8, mip, zero
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mie!" # Machine Interrupt Enable
# -----------------------------------------------------------------------------
  csrrw zero, mie, x8
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mip!" # Machine Interrupt Pending
# -----------------------------------------------------------------------------
  csrrw zero, mip, x8
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mtime64"
# -----------------------------------------------------------------------------
  pushdatos

1:li x14, 0x10020000
  lw x15, 4(x14)
  lw x8, 0(x14)
  lw x14, 4(x14)
  bne x15, x14, 1b

  pushda x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mtimecmp64"
# -----------------------------------------------------------------------------
  pushdatos

1:li x14, 0x10020008
  lw x8, 0(x14)
  lw x14, 4(x14)

  pushda x14
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "mtimecmp64!" # ( ud -- )
# -----------------------------------------------------------------------------

  li x14, 0x10020008
  # As we are doing 32 bit writes, an intermediate mtimecmp value may cause spurious interrupts.
  # Prevent that by first setting the dummy MSB to an unacheivable value
  li x15, 0xffffffff
  sc x15, 4(x14)

  popda x15
  sc x15, 4(x14)
  sc x8, 0(x14)

  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cycles64" # Uptime in cycles, 64 bits
cycles64:
# -----------------------------------------------------------------------------
  pushdatos
1:
  csrrs x15, 0xB80, zero         # Hi order
  csrrs x8, mcycle, zero        # Lo order
  csrrs x14, 0xB80, zero         # Hi order again
  bne x15, x14, 1b                # If hi orders not equal, do it again
  pushda x15
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "cycles" # Uptime in cycles, 32 bits
cycles:
# -----------------------------------------------------------------------------
  pushdatos
  li x14, 0x0200BFF8
  lw x8, 0(x14)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "unhandled" # Message for wild interrupts
unhandled:                             #   and handler for unused interrupts
# -----------------------------------------------------------------------------
  push x1
  write "Unhandled Interrupt "
  call interrupt_cause
  call hexdot
  writeln "!"
  pop x1
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "fault" # Message for unhandled exceptions
fault:
# -----------------------------------------------------------------------------
  push x1
  # write "Unhandled Exception "
  write "Unhandled Interrupt "
  call interrupt_cause
  call hexdot
  write "at "
  call interrupt_location
  call hexdot
  writeln "!"
  pop x1
  ret

# -----------------------------------------------------------------------------
  .include "irq-handler.s"
# -----------------------------------------------------------------------------

  initinterrupt dfx, _dfx_irq_handler, unhandled
  initinterrupt uart, _uart_irq_handler, unhandled
  initinterrupt i2c, _i2c_irq_handler, unhandled
  initinterrupt usb_hid_0, _usb_hid_0_irq_handler, unhandled
  initinterrupt usb_hid_1, _usb_hid_1_irq_handler, unhandled
  initinterrupt gpio, _gpio_irq_handler, unhandled
  initinterrupt sdspi, _sdspi_irq_handler, unhandled
  initinterrupt vs0, _vs0_irq_handler, unhandled
  initinterrupt vera, _vera_irq_handler, unhandled
  initinterrupt timer, _timer_irq_handler, unhandled

