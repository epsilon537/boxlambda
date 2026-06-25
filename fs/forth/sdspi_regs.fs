\ --- SDSPI
\ --- 32-bit register bitfield accessors

$10000020 constant SDSPI_BASE_ADDR


\ CMD - Command and status register
SDSPI_BASE_ADDR $0 + constant SDSPI_CMD_ADDR

\ -- Address Mask Offset
SDSPI_CMD_ADDR $ff #0 bitfield@ SDSPI_CMD_R1_CMD@
SDSPI_CMD_ADDR $ff #0 bitfield! SDSPI_CMD_R1_CMD!
SDSPI_CMD_ADDR $300 #8 bitfield@ SDSPI_CMD_ERESP@
SDSPI_CMD_ADDR $300 #8 bitfield! SDSPI_CMD_ERESP!

  #0 constant SDSPI_CMD_ERESP_R1_RESP \ Expect R1 response. 
  #1 constant SDSPI_CMD_ERESP_R1B_RESP \ Expect R1b response. 
  #2 constant SDSPI_CMD_ERESP_R2_R3_R7 \ Expect R2, R3, R7 32-bit response. 

SDSPI_CMD_ADDR $400 #10 bitfield@ SDSPI_CMD_WR@
SDSPI_CMD_ADDR $400 #10 bitfield! SDSPI_CMD_WR!
SDSPI_CMD_ADDR $800 #11 bitfield@ SDSPI_CMD_F@
SDSPI_CMD_ADDR $800 #11 bitfield! SDSPI_CMD_F!
SDSPI_CMD_ADDR $1000 #12 bitfield@ SDSPI_CMD_SEL@
SDSPI_CMD_ADDR $1000 #12 bitfield! SDSPI_CMD_SEL!
SDSPI_CMD_ADDR $4000 #14 bitfield@ SDSPI_CMD_BUSY@
SDSPI_CMD_ADDR $8000 #15 bitfield@ SDSPI_CMD_ERR@
SDSPI_CMD_ADDR $8000 #15 bitfield! SDSPI_CMD_ERR!
SDSPI_CMD_ADDR $40000 #18 bitfield@ SDSPI_CMD_REM@
SDSPI_CMD_ADDR $40000 #18 bitfield! SDSPI_CMD_REM!
SDSPI_CMD_ADDR $80000 #19 bitfield@ SDSPI_CMD_P@

\ DAT - Return data/argument register
SDSPI_BASE_ADDR $4 + constant SDSPI_DAT_ADDR

\ -- Address Mask Offset
SDSPI_DAT_ADDR $ffffffff #0 bitfield@ SDSPI_DAT_VALUE@
SDSPI_DAT_ADDR $ffffffff #0 bitfield! SDSPI_DAT_VALUE!

\ FIFO_0 - 128 word FIFO[0] data
SDSPI_BASE_ADDR $8 + constant SDSPI_FIFO_0_ADDR

\ -- Address Mask Offset
SDSPI_FIFO_0_ADDR $ffffffff #0 bitfield@ SDSPI_FIFO_0_VALUE@
SDSPI_FIFO_0_ADDR $ffffffff #0 bitfield! SDSPI_FIFO_0_VALUE!

\ FIFO_1 - 128 word FIFO[1] data
SDSPI_BASE_ADDR $c + constant SDSPI_FIFO_1_ADDR

\ -- Address Mask Offset
SDSPI_FIFO_1_ADDR $ffffffff #0 bitfield@ SDSPI_FIFO_1_VALUE@
SDSPI_FIFO_1_ADDR $ffffffff #0 bitfield! SDSPI_FIFO_1_VALUE!

\ ISR - Interrupt status register
SDSPI_BASE_ADDR $10 + constant SDSPI_ISR_ADDR

\ -- Address Mask Offset
SDSPI_ISR_ADDR $1 #0 bitfield@ SDSPI_ISR_BUSY@
SDSPI_ISR_ADDR $1 #0 bitfield! SDSPI_ISR_BUSY!
SDSPI_ISR_ADDR $2 #1 bitfield@ SDSPI_ISR_CARD_REMOVED@
SDSPI_ISR_ADDR $2 #1 bitfield! SDSPI_ISR_CARD_REMOVED!

\ IEN - Interrupt enable register
SDSPI_BASE_ADDR $14 + constant SDSPI_IEN_ADDR

\ -- Address Mask Offset
SDSPI_IEN_ADDR $1 #0 bitfield@ SDSPI_IEN_BUSY@
SDSPI_IEN_ADDR $1 #0 bitfield! SDSPI_IEN_BUSY!
SDSPI_IEN_ADDR $2 #1 bitfield@ SDSPI_IEN_CARD_REMOVED@
SDSPI_IEN_ADDR $2 #1 bitfield! SDSPI_IEN_CARD_REMOVED!
