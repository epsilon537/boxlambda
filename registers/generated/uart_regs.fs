\ --- UART

$10010000 constant UART_BASE_ADDR

\ SETUP - Setup register
$0 constant UART_SETUP_ADDR

\ SETUP.BAUD_CLKS - System clock per baudrate interval
24 constant UART_SETUP_BAUD_CLKS_WIDTH
0 constant UART_SETUP_BAUD_CLKS_LSB
$ffffff constant UART_SETUP_BAUD_CLKS_MASK
\ SETUP.PFT - Parity setup
3 constant UART_SETUP_PFT_WIDTH
24 constant UART_SETUP_PFT_LSB
$7000000 constant UART_SETUP_PFT_MASK
\ SETUP.S - Number of stop bits - 1
1 constant UART_SETUP_S_WIDTH
27 constant UART_SETUP_S_LSB
$8000000 constant UART_SETUP_S_MASK
\ SETUP.N - 8 - number of bits per word
2 constant UART_SETUP_N_WIDTH
28 constant UART_SETUP_N_LSB
$30000000 constant UART_SETUP_N_MASK
\ SETUP.H - Disable hardware flow control
1 constant UART_SETUP_H_WIDTH
30 constant UART_SETUP_H_LSB
$40000000 constant UART_SETUP_H_MASK
\ FIFO - Rx and Tx FIFO size and status
$4 constant UART_FIFO_ADDR

\ FIFO.RX_Z - Data available in Rx FIFO.
1 constant UART_FIFO_RX_Z_WIDTH
0 constant UART_FIFO_RX_Z_LSB
$1 constant UART_FIFO_RX_Z_MASK
\ FIFO.RX_H - Rx FIFO high order fill bit set.
1 constant UART_FIFO_RX_H_WIDTH
1 constant UART_FIFO_RX_H_LSB
$2 constant UART_FIFO_RX_H_MASK
\ FIFO.RX_FILL - Number of filled entries in Rx FIFO.
10 constant UART_FIFO_RX_FILL_WIDTH
2 constant UART_FIFO_RX_FILL_LSB
$ffc constant UART_FIFO_RX_FILL_MASK
\ FIFO.RX_LGLN - Log base 2 of FIFO length.
4 constant UART_FIFO_RX_LGLN_WIDTH
12 constant UART_FIFO_RX_LGLN_LSB
$f000 constant UART_FIFO_RX_LGLN_MASK
\ FIFO.TX_Z - Space is available in Tx FIFO.
1 constant UART_FIFO_TX_Z_WIDTH
16 constant UART_FIFO_TX_Z_LSB
$10000 constant UART_FIFO_TX_Z_MASK
\ FIFO.TX_H - Tx FIFO high order fill bit set.
1 constant UART_FIFO_TX_H_WIDTH
17 constant UART_FIFO_TX_H_LSB
$20000 constant UART_FIFO_TX_H_MASK
\ FIFO.TX_AVL - Number of available spaces in Tx FIFO.
10 constant UART_FIFO_TX_AVL_WIDTH
18 constant UART_FIFO_TX_AVL_LSB
$ffc0000 constant UART_FIFO_TX_AVL_MASK
\ FIFO.TX_LGLN - Log base 2 of FIFO length.
4 constant UART_FIFO_TX_LGLN_WIDTH
28 constant UART_FIFO_TX_LGLN_LSB
$f0000000 constant UART_FIFO_TX_LGLN_MASK
\ RXDATA - Rx data register.
$8 constant UART_RXDATA_ADDR

\ RXDATA.RWORD - Read data word.
8 constant UART_RXDATA_RWORD_WIDTH
0 constant UART_RXDATA_RWORD_LSB
$ff constant UART_RXDATA_RWORD_MASK
\ RXDATA.S - Data invalid.
1 constant UART_RXDATA_S_WIDTH
8 constant UART_RXDATA_S_LSB
$100 constant UART_RXDATA_S_MASK
\ RXDATA.P - Parity error.
1 constant UART_RXDATA_P_WIDTH
9 constant UART_RXDATA_P_LSB
$200 constant UART_RXDATA_P_MASK
\ RXDATA.F - Frame error.
1 constant UART_RXDATA_F_WIDTH
10 constant UART_RXDATA_F_LSB
$400 constant UART_RXDATA_F_MASK
\ RXDATA.B - Rx line is in break condition.
1 constant UART_RXDATA_B_WIDTH
11 constant UART_RXDATA_B_LSB
$800 constant UART_RXDATA_B_MASK
\ RXDATA.E - Read indicates Rx FIFO has overflowed since last reset. Writing 1 clears FIFO and waits for line idle before receiving next byte.
1 constant UART_RXDATA_E_WIDTH
12 constant UART_RXDATA_E_LSB
$1000 constant UART_RXDATA_E_MASK
\ TXDATA - Tx data register.
$c constant UART_TXDATA_ADDR

\ TXDATA.TWORD - Transmit data word.
8 constant UART_TXDATA_TWORD_WIDTH
0 constant UART_TXDATA_TWORD_LSB
$ff constant UART_TXDATA_TWORD_MASK
\ TXDATA.S - Transmit busy.
1 constant UART_TXDATA_S_WIDTH
8 constant UART_TXDATA_S_LSB
$100 constant UART_TXDATA_S_MASK
\ TXDATA.B - Tx break condition.
1 constant UART_TXDATA_B_WIDTH
9 constant UART_TXDATA_B_LSB
$200 constant UART_TXDATA_B_MASK
\ TXDATA.E - Read indicates Tx FIFO has overflowed since last reset. Writing 1 resets FIFO.
1 constant UART_TXDATA_E_WIDTH
12 constant UART_TXDATA_E_LSB
$1000 constant UART_TXDATA_E_MASK
\ TXDATA.Z - Tx FIFO not full.
1 constant UART_TXDATA_Z_WIDTH
13 constant UART_TXDATA_Z_LSB
$2000 constant UART_TXDATA_Z_MASK
\ TXDATA.H - Tx FIFO at least half full.
1 constant UART_TXDATA_H_WIDTH
14 constant UART_TXDATA_H_LSB
$4000 constant UART_TXDATA_H_MASK
\ TXDATA.R - Received RTS instantaneous value.
1 constant UART_TXDATA_R_WIDTH
15 constant UART_TXDATA_R_LSB
$8000 constant UART_TXDATA_R_MASK
\ ISR - Interrupt status register
$10 constant UART_ISR_ADDR

\ ISR.RX_DATA_AVL - Rx FIFO went from empty to non-empty state.
1 constant UART_ISR_RX_DATA_AVL_WIDTH
0 constant UART_ISR_RX_DATA_AVL_LSB
$1 constant UART_ISR_RX_DATA_AVL_MASK
\ ISR.RX_FIFO_HALF_FULL - Receive FIFO passed the half-full threshold.
1 constant UART_ISR_RX_FIFO_HALF_FULL_WIDTH
1 constant UART_ISR_RX_FIFO_HALF_FULL_LSB
$2 constant UART_ISR_RX_FIFO_HALF_FULL_MASK
\ ISR.TX_FIFO_EMPTY - Tx FIFO went from non-empty to empty state.
1 constant UART_ISR_TX_FIFO_EMPTY_WIDTH
2 constant UART_ISR_TX_FIFO_EMPTY_LSB
$4 constant UART_ISR_TX_FIFO_EMPTY_MASK
\ ISR.TX_FIFO_HALF_EMPTY - Tx FIFO filling level dropped below the half empty threshold.
1 constant UART_ISR_TX_FIFO_HALF_EMPTY_WIDTH
3 constant UART_ISR_TX_FIFO_HALF_EMPTY_LSB
$8 constant UART_ISR_TX_FIFO_HALF_EMPTY_MASK
\ IEN - Interrupt enable register
$14 constant UART_IEN_ADDR

\ IEN.RX_DATA_AVL - Enable RX_DATA_AVL interrupt.
1 constant UART_IEN_RX_DATA_AVL_WIDTH
0 constant UART_IEN_RX_DATA_AVL_LSB
$1 constant UART_IEN_RX_DATA_AVL_MASK
\ IEN.RX_FIFO_HALF_FULL - Enable RX_FIFO_HALF_FULL interrupt.
1 constant UART_IEN_RX_FIFO_HALF_FULL_WIDTH
1 constant UART_IEN_RX_FIFO_HALF_FULL_LSB
$2 constant UART_IEN_RX_FIFO_HALF_FULL_MASK
\ IEN.TX_FIFO_EMPTY - Enable TX_FIFO_EMPTY interrupt.
1 constant UART_IEN_TX_FIFO_EMPTY_WIDTH
2 constant UART_IEN_TX_FIFO_EMPTY_LSB
$4 constant UART_IEN_TX_FIFO_EMPTY_MASK
\ IEN.TX_FIFO_HALF_EMPTY - Enabled TX_FIFO_HALF_EMPTY interrupt.
1 constant UART_IEN_TX_FIFO_HALF_EMPTY_WIDTH
3 constant UART_IEN_TX_FIFO_HALF_EMPTY_LSB
$8 constant UART_IEN_TX_FIFO_HALF_EMPTY_MASK
