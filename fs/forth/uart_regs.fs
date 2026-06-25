\ --- UART
\ --- 32-bit register bitfield accessors

$10010000 constant UART_BASE_ADDR


\ SETUP - Setup register
UART_BASE_ADDR $0 + constant UART_SETUP_ADDR

\ -- Address Mask Offset
UART_SETUP_ADDR $ffffff #0 bitfield@ UART_SETUP_BAUD_CLKS@
UART_SETUP_ADDR $ffffff #0 bitfield! UART_SETUP_BAUD_CLKS!

  #434 constant UART_SETUP_BAUD_CLKS_B115200 \ 115200 baud 
  #5208 constant UART_SETUP_BAUD_CLKS_B9600 \ 9600 baud 

UART_SETUP_ADDR $7000000 #24 bitfield@ UART_SETUP_PFT@
UART_SETUP_ADDR $7000000 #24 bitfield! UART_SETUP_PFT!

  #0 constant UART_SETUP_PFT_P_NONE \ No parity 
  #4 constant UART_SETUP_PFT_P_ODD \ Odd Parity 
  #5 constant UART_SETUP_PFT_P_EVEN \ Even Parity 
  #6 constant UART_SETUP_PFT_P_SPC \ Party bit is a space 
  #7 constant UART_SETUP_PFT_P_MK \ Party bit is a mark 

UART_SETUP_ADDR $8000000 #27 bitfield@ UART_SETUP_S@
UART_SETUP_ADDR $8000000 #27 bitfield! UART_SETUP_S!

  #0 constant UART_SETUP_S_STOP_1 \ One stop bit 
  #1 constant UART_SETUP_S_STOP_2 \ Two stop bits 

UART_SETUP_ADDR $30000000 #28 bitfield@ UART_SETUP_N@
UART_SETUP_ADDR $30000000 #28 bitfield! UART_SETUP_N!

  #0 constant UART_SETUP_N_BPW_8 \ 8 bits per word 
  #1 constant UART_SETUP_N_BPW_7 \ 7 bits per word 
  #2 constant UART_SETUP_N_BPW_6 \ 6 bits per word 
  #3 constant UART_SETUP_N_BPW_5 \ 5 bits per word 

UART_SETUP_ADDR $40000000 #30 bitfield@ UART_SETUP_H@
UART_SETUP_ADDR $40000000 #30 bitfield! UART_SETUP_H!

  #0 constant UART_SETUP_H_HFL_EN \ Hardware flow control enabled 
  #1 constant UART_SETUP_H_HFL_DIS \ Hardware flow control disabled 


\ FIFO - Rx and Tx FIFO size and status
UART_BASE_ADDR $4 + constant UART_FIFO_ADDR

\ -- Address Mask Offset
UART_FIFO_ADDR $1 #0 bitfield@ UART_FIFO_RX_Z@
UART_FIFO_ADDR $2 #1 bitfield@ UART_FIFO_RX_H@
UART_FIFO_ADDR $ffc #2 bitfield@ UART_FIFO_RX_FILL@
UART_FIFO_ADDR $f000 #12 bitfield@ UART_FIFO_RX_LGLN@
UART_FIFO_ADDR $10000 #16 bitfield@ UART_FIFO_TX_Z@
UART_FIFO_ADDR $20000 #17 bitfield@ UART_FIFO_TX_H@
UART_FIFO_ADDR $ffc0000 #18 bitfield@ UART_FIFO_TX_AVL@
UART_FIFO_ADDR $f0000000 #28 bitfield@ UART_FIFO_TX_LGLN@

\ RXDATA - Rx data register.
UART_BASE_ADDR $8 + constant UART_RXDATA_ADDR

\ -- Address Mask Offset
UART_RXDATA_ADDR $ff #0 bitfield@ UART_RXDATA_RWORD@
UART_RXDATA_ADDR $100 #8 bitfield@ UART_RXDATA_S@

  #0 constant UART_RXDATA_S_RWORD_VALID \ RWORD is valid. 
  #1 constant UART_RXDATA_S_RWORD_INVALID \ RWORD is invalid. 

UART_RXDATA_ADDR $200 #9 bitfield@ UART_RXDATA_P@
UART_RXDATA_ADDR $200 #9 bitfield! UART_RXDATA_P!
UART_RXDATA_ADDR $400 #10 bitfield@ UART_RXDATA_F@
UART_RXDATA_ADDR $400 #10 bitfield! UART_RXDATA_F!
UART_RXDATA_ADDR $800 #11 bitfield@ UART_RXDATA_B@
UART_RXDATA_ADDR $800 #11 bitfield! UART_RXDATA_B!
UART_RXDATA_ADDR $1000 #12 bitfield@ UART_RXDATA_E@
UART_RXDATA_ADDR $1000 #12 bitfield! UART_RXDATA_E!

\ TXDATA - Tx data register.
UART_BASE_ADDR $c + constant UART_TXDATA_ADDR

\ -- Address Mask Offset
UART_TXDATA_ADDR $ff #0 bitfield@ UART_TXDATA_TWORD@
UART_TXDATA_ADDR $ff #0 bitfield! UART_TXDATA_TWORD!
UART_TXDATA_ADDR $100 #8 bitfield@ UART_TXDATA_S@
UART_TXDATA_ADDR $200 #9 bitfield@ UART_TXDATA_B@
UART_TXDATA_ADDR $200 #9 bitfield! UART_TXDATA_B!
UART_TXDATA_ADDR $1000 #12 bitfield@ UART_TXDATA_E@
UART_TXDATA_ADDR $1000 #12 bitfield! UART_TXDATA_E!
UART_TXDATA_ADDR $2000 #13 bitfield@ UART_TXDATA_Z@
UART_TXDATA_ADDR $4000 #14 bitfield@ UART_TXDATA_H@
UART_TXDATA_ADDR $8000 #15 bitfield@ UART_TXDATA_R@

\ ISR - Interrupt status register
UART_BASE_ADDR $10 + constant UART_ISR_ADDR

\ -- Address Mask Offset
UART_ISR_ADDR $1 #0 bitfield@ UART_ISR_RX_DATA_AVL@
UART_ISR_ADDR $1 #0 bitfield! UART_ISR_RX_DATA_AVL!
UART_ISR_ADDR $2 #1 bitfield@ UART_ISR_RX_FIFO_HALF_FULL@
UART_ISR_ADDR $2 #1 bitfield! UART_ISR_RX_FIFO_HALF_FULL!
UART_ISR_ADDR $4 #2 bitfield@ UART_ISR_TX_FIFO_EMPTY@
UART_ISR_ADDR $4 #2 bitfield! UART_ISR_TX_FIFO_EMPTY!
UART_ISR_ADDR $8 #3 bitfield@ UART_ISR_TX_FIFO_HALF_EMPTY@
UART_ISR_ADDR $8 #3 bitfield! UART_ISR_TX_FIFO_HALF_EMPTY!

\ IEN - Interrupt enable register
UART_BASE_ADDR $14 + constant UART_IEN_ADDR

\ -- Address Mask Offset
UART_IEN_ADDR $1 #0 bitfield@ UART_IEN_RX_DATA_AVL@
UART_IEN_ADDR $1 #0 bitfield! UART_IEN_RX_DATA_AVL!
UART_IEN_ADDR $2 #1 bitfield@ UART_IEN_RX_FIFO_HALF_FULL@
UART_IEN_ADDR $2 #1 bitfield! UART_IEN_RX_FIFO_HALF_FULL!
UART_IEN_ADDR $4 #2 bitfield@ UART_IEN_TX_FIFO_EMPTY@
UART_IEN_ADDR $4 #2 bitfield! UART_IEN_TX_FIFO_EMPTY!
UART_IEN_ADDR $8 #3 bitfield@ UART_IEN_TX_FIFO_HALF_EMPTY@
UART_IEN_ADDR $8 #3 bitfield! UART_IEN_TX_FIFO_HALF_EMPTY!
