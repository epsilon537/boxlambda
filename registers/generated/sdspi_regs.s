# --- SDSPI

.equ SDSPI_BASE_ADDR, 0x10000020

# CMD - Command and status register
.equ SDSPI_CMD_ADDR, 0x0

# CMD.R1_CMD - On write, if bits [7:6]==01 and card idle, remaining bits are sent to card. Contains R1 response when command has completed.
.equ SDSPI_CMD_R1_CMD_WIDTH 8
.equ SDSPI_CMD_R1_CMD_LSB 0
.equ SDSPI_CMD_R1_CMD_MASK 0xff
# CMD.ERESP - Expected response.
.equ SDSPI_CMD_ERESP_WIDTH 2
.equ SDSPI_CMD_ERESP_LSB 8
.equ SDSPI_CMD_ERESP_MASK 0x300
# CMD.WR - 1 to write from FIFO to card, 0 to read from card into FIFO. Assumes F is set.
.equ SDSPI_CMD_WR_WIDTH 1
.equ SDSPI_CMD_WR_LSB 10
.equ SDSPI_CMD_WR_MASK 0x400
# CMD.F - Set if FIFO data transmision accompanies command.
.equ SDSPI_CMD_F_WIDTH 1
.equ SDSPI_CMD_F_LSB 11
.equ SDSPI_CMD_F_MASK 0x800
# CMD.SEL - 1 = select FIFO[0], 0 = select FIFO[1].
.equ SDSPI_CMD_SEL_WIDTH 1
.equ SDSPI_CMD_SEL_LSB 12
.equ SDSPI_CMD_SEL_MASK 0x1000
# CMD.BUSY - Busy bit, set while command is running.
.equ SDSPI_CMD_BUSY_WIDTH 1
.equ SDSPI_CMD_BUSY_LSB 14
.equ SDSPI_CMD_BUSY_MASK 0x4000
# CMD.ERR - Error indication. Time-out, card reset, CRC error, R1 response error.
.equ SDSPI_CMD_ERR_WIDTH 1
.equ SDSPI_CMD_ERR_LSB 15
.equ SDSPI_CMD_ERR_MASK 0x8000
# CMD.REM - Card has been removed since last read. If P=0 and R=1, card has been inserted and needs initialization.
.equ SDSPI_CMD_REM_WIDTH 1
.equ SDSPI_CMD_REM_LSB 18
.equ SDSPI_CMD_REM_MASK 0x40000
# CMD.P - 1 = card missing, 0 = card present.
.equ SDSPI_CMD_P_WIDTH 1
.equ SDSPI_CMD_P_LSB 19
.equ SDSPI_CMD_P_MASK 0x80000
# DAT - Return data/argument register
.equ SDSPI_DAT_ADDR, 0x4

# DAT.VALUE - Command argument, command response. R2 response is in upper 8-bits.
.equ SDSPI_DAT_VALUE_WIDTH 32
.equ SDSPI_DAT_VALUE_LSB 0
.equ SDSPI_DAT_VALUE_MASK 0xffffffff
# FIFO_0 - 128 word FIFO[0] data
.equ SDSPI_FIFO_0_ADDR, 0x8

# FIFO_0.VALUE - Data read from or to write to card.
.equ SDSPI_FIFO_0_VALUE_WIDTH 32
.equ SDSPI_FIFO_0_VALUE_LSB 0
.equ SDSPI_FIFO_0_VALUE_MASK 0xffffffff
# FIFO_1 - 128 word FIFO[1] data
.equ SDSPI_FIFO_1_ADDR, 0xc

# FIFO_1.VALUE - Data read from or to write to card.
.equ SDSPI_FIFO_1_VALUE_WIDTH 32
.equ SDSPI_FIFO_1_VALUE_LSB 0
.equ SDSPI_FIFO_1_VALUE_MASK 0xffffffff
# ISR - Interrupt status register
.equ SDSPI_ISR_ADDR, 0x10

# ISR.BUSY - Set when controller goes from busy to non-busy state
.equ SDSPI_ISR_BUSY_WIDTH 1
.equ SDSPI_ISR_BUSY_LSB 0
.equ SDSPI_ISR_BUSY_MASK 0x1
# ISR.CARD_REMOVED - Set when controller detects that the SD card has been removed.
.equ SDSPI_ISR_CARD_REMOVED_WIDTH 1
.equ SDSPI_ISR_CARD_REMOVED_LSB 1
.equ SDSPI_ISR_CARD_REMOVED_MASK 0x2
# IEN - Interrupt enable register
.equ SDSPI_IEN_ADDR, 0x14

# IEN.BUSY - Set to enable BUSY interrupt.
.equ SDSPI_IEN_BUSY_WIDTH 1
.equ SDSPI_IEN_BUSY_LSB 0
.equ SDSPI_IEN_BUSY_MASK 0x1
# IEN.CARD_REMOVED - Set to enabled CARD_REMOVED interrupt.
.equ SDSPI_IEN_CARD_REMOVED_WIDTH 1
.equ SDSPI_IEN_CARD_REMOVED_LSB 1
.equ SDSPI_IEN_CARD_REMOVED_MASK 0x2
