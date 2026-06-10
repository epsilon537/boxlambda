\ --- SDSPI

$10000020 constant SDSPI_BASE_ADDR

\ CMD - Command and status register
$0 constant SDSPI_CMD_ADDR

\ CMD.R1_CMD - On write, if bits [7:6]==01 and card idle, remaining bits are sent to card. Contains R1 response when command has completed.
8 constant SDSPI_CMD_R1_CMD_WIDTH
0 constant SDSPI_CMD_R1_CMD_LSB
$ff constant SDSPI_CMD_R1_CMD_MASK
\ CMD.ERESP - Expected response.
2 constant SDSPI_CMD_ERESP_WIDTH
8 constant SDSPI_CMD_ERESP_LSB
$300 constant SDSPI_CMD_ERESP_MASK
\ CMD.WR - 1 to write from FIFO to card, 0 to read from card into FIFO. Assumes F is set.
1 constant SDSPI_CMD_WR_WIDTH
10 constant SDSPI_CMD_WR_LSB
$400 constant SDSPI_CMD_WR_MASK
\ CMD.F - Set if FIFO data transmission accompanies command.
1 constant SDSPI_CMD_F_WIDTH
11 constant SDSPI_CMD_F_LSB
$800 constant SDSPI_CMD_F_MASK
\ CMD.SEL - 1 = select FIFO[0], 0 = select FIFO[1].
1 constant SDSPI_CMD_SEL_WIDTH
12 constant SDSPI_CMD_SEL_LSB
$1000 constant SDSPI_CMD_SEL_MASK
\ CMD.BUSY - Busy bit, set while command is running.
1 constant SDSPI_CMD_BUSY_WIDTH
14 constant SDSPI_CMD_BUSY_LSB
$4000 constant SDSPI_CMD_BUSY_MASK
\ CMD.ERR - Error indication. Time-out, card reset, CRC error, R1 response error.
1 constant SDSPI_CMD_ERR_WIDTH
15 constant SDSPI_CMD_ERR_LSB
$8000 constant SDSPI_CMD_ERR_MASK
\ CMD.REM - Card has been removed since last read. If P=0 and R=1, card has been inserted and needs initialization.
1 constant SDSPI_CMD_REM_WIDTH
18 constant SDSPI_CMD_REM_LSB
$40000 constant SDSPI_CMD_REM_MASK
\ CMD.P - 1 = card missing, 0 = card present.
1 constant SDSPI_CMD_P_WIDTH
19 constant SDSPI_CMD_P_LSB
$80000 constant SDSPI_CMD_P_MASK
\ DAT - Return data/argument register
$4 constant SDSPI_DAT_ADDR

\ DAT.VALUE - Command argument, command response. R2 response is in upper 8-bits.
32 constant SDSPI_DAT_VALUE_WIDTH
0 constant SDSPI_DAT_VALUE_LSB
$ffffffff constant SDSPI_DAT_VALUE_MASK
\ FIFO_0 - 128 word FIFO[0] data
$8 constant SDSPI_FIFO_0_ADDR

\ FIFO_0.VALUE - Data read from or to write to card.
32 constant SDSPI_FIFO_0_VALUE_WIDTH
0 constant SDSPI_FIFO_0_VALUE_LSB
$ffffffff constant SDSPI_FIFO_0_VALUE_MASK
\ FIFO_1 - 128 word FIFO[1] data
$c constant SDSPI_FIFO_1_ADDR

\ FIFO_1.VALUE - Data read from or to write to card.
32 constant SDSPI_FIFO_1_VALUE_WIDTH
0 constant SDSPI_FIFO_1_VALUE_LSB
$ffffffff constant SDSPI_FIFO_1_VALUE_MASK
\ ISR - Interrupt status register
$10 constant SDSPI_ISR_ADDR

\ ISR.BUSY - Set when controller goes from busy to non-busy state
1 constant SDSPI_ISR_BUSY_WIDTH
0 constant SDSPI_ISR_BUSY_LSB
$1 constant SDSPI_ISR_BUSY_MASK
\ ISR.CARD_REMOVED - Set when controller detects that the SD card has been removed.
1 constant SDSPI_ISR_CARD_REMOVED_WIDTH
1 constant SDSPI_ISR_CARD_REMOVED_LSB
$2 constant SDSPI_ISR_CARD_REMOVED_MASK
\ IEN - Interrupt enable register
$14 constant SDSPI_IEN_ADDR

\ IEN.BUSY - Set to enable BUSY interrupt.
1 constant SDSPI_IEN_BUSY_WIDTH
0 constant SDSPI_IEN_BUSY_LSB
$1 constant SDSPI_IEN_BUSY_MASK
\ IEN.CARD_REMOVED - Set to enabled CARD_REMOVED interrupt.
1 constant SDSPI_IEN_CARD_REMOVED_WIDTH
1 constant SDSPI_IEN_CARD_REMOVED_LSB
$2 constant SDSPI_IEN_CARD_REMOVED_MASK
