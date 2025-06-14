// Created with Corsair v1.0.4
#ifndef __UART_REGS_H
#define __UART_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define UART_BASE_ADDR 0x10010000

// SETUP - Setup register
#define UART_SETUP_ADDR 0x0
#define UART_SETUP_RESET 0x1b2
typedef struct {
    uint32_t BAUD_CLKS : 24; // System clock per baudrate interval
    uint32_t PFT : 3; // Parity setup
    uint32_t S : 1; // Number of stop bits - 1
    uint32_t N : 2; // 8 - number of bits per word
    uint32_t H : 1; // Disable hardware flow control
} uart_setup_t;

// SETUP.BAUD_CLKS - System clock per baudrate interval
#define UART_SETUP_BAUD_CLKS_WIDTH 24
#define UART_SETUP_BAUD_CLKS_LSB 0
#define UART_SETUP_BAUD_CLKS_MASK 0xffffff
#define UART_SETUP_BAUD_CLKS_RESET 0x1b2
typedef enum {
    UART_SETUP_BAUD_CLKS_B115200 = 0x1b2, //115200 baud
    UART_SETUP_BAUD_CLKS_B9600 = 0x1458, //9600 baud
} uart_setup_baud_clks_t;

// SETUP.PFT - Parity setup
#define UART_SETUP_PFT_WIDTH 3
#define UART_SETUP_PFT_LSB 24
#define UART_SETUP_PFT_MASK 0x7000000
#define UART_SETUP_PFT_RESET 0x0
typedef enum {
    UART_SETUP_PFT_P_NONE = 0x0, //No parity
    UART_SETUP_PFT_P_ODD = 0x4, //Odd Parity
    UART_SETUP_PFT_P_EVEN = 0x5, //Even Parity
    UART_SETUP_PFT_P_SPC = 0x6, //Party bit is a space
    UART_SETUP_PFT_P_MK = 0x7, //Party bit is a mark
} uart_setup_pft_t;

// SETUP.S - Number of stop bits - 1
#define UART_SETUP_S_WIDTH 1
#define UART_SETUP_S_LSB 27
#define UART_SETUP_S_MASK 0x8000000
#define UART_SETUP_S_RESET 0x0
typedef enum {
    UART_SETUP_S_STOP_1 = 0x0, //One stop bit
    UART_SETUP_S_STOP_2 = 0x1, //Two stop bits
} uart_setup_s_t;

// SETUP.N - 8 - number of bits per word
#define UART_SETUP_N_WIDTH 2
#define UART_SETUP_N_LSB 28
#define UART_SETUP_N_MASK 0x30000000
#define UART_SETUP_N_RESET 0x0
typedef enum {
    UART_SETUP_N_BPW_8 = 0x0, //8 bits per word
    UART_SETUP_N_BPW_7 = 0x1, //7 bits per word
    UART_SETUP_N_BPW_6 = 0x2, //6 bits per word
    UART_SETUP_N_BPW_5 = 0x3, //5 bits per word
} uart_setup_n_t;

// SETUP.H - Disable hardware flow control
#define UART_SETUP_H_WIDTH 1
#define UART_SETUP_H_LSB 30
#define UART_SETUP_H_MASK 0x40000000
#define UART_SETUP_H_RESET 0x0
typedef enum {
    UART_SETUP_H_HFL_EN = 0x0, //Hardware flow control enabled
    UART_SETUP_H_HFL_DIS = 0x1, //Hardware flow control disabled
} uart_setup_h_t;

// FIFO - Rx and Tx FIFO size and status
#define UART_FIFO_ADDR 0x4
#define UART_FIFO_RESET 0x0
typedef struct {
    uint32_t RX_Z : 1; // Data available in Rx FIFO.
    uint32_t RX_H : 1; // Rx FIFO high order fill bit set.
    uint32_t RX_FILL : 10; // Number of filled entries in Rx FIFO.
    uint32_t RX_LGLN : 4; // Log base 2 of FIFO length.
    uint32_t TX_Z : 1; // Space available in Tx FIFO.
    uint32_t TX_H : 1; // Tx FIFO high order fill bit set.
    uint32_t TX_FILL : 10; // Number of available spaces in Tx FIFO.
    uint32_t TX_LGLN : 4; // Log base 2 of FIFO length.
} uart_fifo_t;

// FIFO.RX_Z - Data available in Rx FIFO.
#define UART_FIFO_RX_Z_WIDTH 1
#define UART_FIFO_RX_Z_LSB 0
#define UART_FIFO_RX_Z_MASK 0x1
#define UART_FIFO_RX_Z_RESET 0x0

// FIFO.RX_H - Rx FIFO high order fill bit set.
#define UART_FIFO_RX_H_WIDTH 1
#define UART_FIFO_RX_H_LSB 1
#define UART_FIFO_RX_H_MASK 0x2
#define UART_FIFO_RX_H_RESET 0x0

// FIFO.RX_FILL - Number of filled entries in Rx FIFO.
#define UART_FIFO_RX_FILL_WIDTH 10
#define UART_FIFO_RX_FILL_LSB 2
#define UART_FIFO_RX_FILL_MASK 0xffc
#define UART_FIFO_RX_FILL_RESET 0x0

// FIFO.RX_LGLN - Log base 2 of FIFO length.
#define UART_FIFO_RX_LGLN_WIDTH 4
#define UART_FIFO_RX_LGLN_LSB 12
#define UART_FIFO_RX_LGLN_MASK 0xf000
#define UART_FIFO_RX_LGLN_RESET 0x0

// FIFO.TX_Z - Space available in Tx FIFO.
#define UART_FIFO_TX_Z_WIDTH 1
#define UART_FIFO_TX_Z_LSB 16
#define UART_FIFO_TX_Z_MASK 0x10000
#define UART_FIFO_TX_Z_RESET 0x0

// FIFO.TX_H - Tx FIFO high order fill bit set.
#define UART_FIFO_TX_H_WIDTH 1
#define UART_FIFO_TX_H_LSB 17
#define UART_FIFO_TX_H_MASK 0x20000
#define UART_FIFO_TX_H_RESET 0x0

// FIFO.TX_FILL - Number of available spaces in Tx FIFO.
#define UART_FIFO_TX_FILL_WIDTH 10
#define UART_FIFO_TX_FILL_LSB 18
#define UART_FIFO_TX_FILL_MASK 0xffc0000
#define UART_FIFO_TX_FILL_RESET 0x0

// FIFO.TX_LGLN - Log base 2 of FIFO length.
#define UART_FIFO_TX_LGLN_WIDTH 4
#define UART_FIFO_TX_LGLN_LSB 28
#define UART_FIFO_TX_LGLN_MASK 0xf0000000
#define UART_FIFO_TX_LGLN_RESET 0x0

// RXDATA - Rx data register.
#define UART_RXDATA_ADDR 0x8
#define UART_RXDATA_RESET 0x0
typedef struct {
    uint32_t RWORD : 8; // Read data word.
    uint32_t S : 1; // Data invalid.
    uint32_t P : 1; // Parity error.
    uint32_t F : 1; // Frame error.
    uint32_t B : 1; // Rx line is in break condition.
    uint32_t E : 1; // Read indicates Rx FIFO has overflowed since last reset. Writing 1 clears FIFO and waits for line idle before receiving next byte.
    uint32_t : 19; // reserved
} uart_rxdata_t;

// RXDATA.RWORD - Read data word.
#define UART_RXDATA_RWORD_WIDTH 8
#define UART_RXDATA_RWORD_LSB 0
#define UART_RXDATA_RWORD_MASK 0xff
#define UART_RXDATA_RWORD_RESET 0x0

// RXDATA.S - Data invalid.
#define UART_RXDATA_S_WIDTH 1
#define UART_RXDATA_S_LSB 8
#define UART_RXDATA_S_MASK 0x100
#define UART_RXDATA_S_RESET 0x0
typedef enum {
    UART_RXDATA_S_RWORD_VALID = 0x0, //RWORD is valid.
    UART_RXDATA_S_RWORD_INVALID = 0x1, //RWORD is invalid.
} uart_rxdata_s_t;

// RXDATA.P - Parity error.
#define UART_RXDATA_P_WIDTH 1
#define UART_RXDATA_P_LSB 9
#define UART_RXDATA_P_MASK 0x200
#define UART_RXDATA_P_RESET 0x0

// RXDATA.F - Frame error.
#define UART_RXDATA_F_WIDTH 1
#define UART_RXDATA_F_LSB 10
#define UART_RXDATA_F_MASK 0x400
#define UART_RXDATA_F_RESET 0x0

// RXDATA.B - Rx line is in break condition.
#define UART_RXDATA_B_WIDTH 1
#define UART_RXDATA_B_LSB 11
#define UART_RXDATA_B_MASK 0x800
#define UART_RXDATA_B_RESET 0x0

// RXDATA.E - Read indicates Rx FIFO has overflowed since last reset. Writing 1 clears FIFO and waits for line idle before receiving next byte.
#define UART_RXDATA_E_WIDTH 1
#define UART_RXDATA_E_LSB 12
#define UART_RXDATA_E_MASK 0x1000
#define UART_RXDATA_E_RESET 0x0

// TXDATA - Tx data register.
#define UART_TXDATA_ADDR 0xc
#define UART_TXDATA_RESET 0x0
typedef struct {
    uint32_t TWORD : 8; // Transmit data word.
    uint32_t S : 1; // Transmit busy.
    uint32_t B : 1; // Tx break condition.
    uint32_t : 2; // reserved
    uint32_t E : 1; // Read indicates Tx FIFO has overflowed since last reset. Writing 1 resets FIFO.
    uint32_t Z : 1; // Tx FIFO not full.
    uint32_t H : 1; // Tx FIFO at least half full.
    uint32_t R : 1; // Received RTS instantaneous value.
    uint32_t : 16; // reserved
} uart_txdata_t;

// TXDATA.TWORD - Transmit data word.
#define UART_TXDATA_TWORD_WIDTH 8
#define UART_TXDATA_TWORD_LSB 0
#define UART_TXDATA_TWORD_MASK 0xff
#define UART_TXDATA_TWORD_RESET 0x0

// TXDATA.S - Transmit busy.
#define UART_TXDATA_S_WIDTH 1
#define UART_TXDATA_S_LSB 8
#define UART_TXDATA_S_MASK 0x100
#define UART_TXDATA_S_RESET 0x0

// TXDATA.B - Tx break condition.
#define UART_TXDATA_B_WIDTH 1
#define UART_TXDATA_B_LSB 9
#define UART_TXDATA_B_MASK 0x200
#define UART_TXDATA_B_RESET 0x0

// TXDATA.E - Read indicates Tx FIFO has overflowed since last reset. Writing 1 resets FIFO.
#define UART_TXDATA_E_WIDTH 1
#define UART_TXDATA_E_LSB 12
#define UART_TXDATA_E_MASK 0x1000
#define UART_TXDATA_E_RESET 0x0

// TXDATA.Z - Tx FIFO not full.
#define UART_TXDATA_Z_WIDTH 1
#define UART_TXDATA_Z_LSB 13
#define UART_TXDATA_Z_MASK 0x2000
#define UART_TXDATA_Z_RESET 0x0

// TXDATA.H - Tx FIFO at least half full.
#define UART_TXDATA_H_WIDTH 1
#define UART_TXDATA_H_LSB 14
#define UART_TXDATA_H_MASK 0x4000
#define UART_TXDATA_H_RESET 0x0

// TXDATA.R - Received RTS instantaneous value.
#define UART_TXDATA_R_WIDTH 1
#define UART_TXDATA_R_LSB 15
#define UART_TXDATA_R_MASK 0x8000
#define UART_TXDATA_R_RESET 0x0

// ISR - Interrupt status register
#define UART_ISR_ADDR 0x10
#define UART_ISR_RESET 0x0
typedef struct {
    uint32_t RX_DATA_AVL : 1; // Rx FIFO went from empty to non-empty state.
    uint32_t RX_FIFO_HALF_FULL : 1; // Receive FIFO passed the half-full threshold.
    uint32_t TX_FIFO_EMPTY : 1; // Tx FIFO went from non-empty to empty state.
    uint32_t TX_FIFO_HALF_EMPTY : 1; // Tx FIFO filling level dropped below the half empty threshold.
    uint32_t : 28; // reserved
} uart_isr_t;

// ISR.RX_DATA_AVL - Rx FIFO went from empty to non-empty state.
#define UART_ISR_RX_DATA_AVL_WIDTH 1
#define UART_ISR_RX_DATA_AVL_LSB 0
#define UART_ISR_RX_DATA_AVL_MASK 0x1
#define UART_ISR_RX_DATA_AVL_RESET 0x0

// ISR.RX_FIFO_HALF_FULL - Receive FIFO passed the half-full threshold.
#define UART_ISR_RX_FIFO_HALF_FULL_WIDTH 1
#define UART_ISR_RX_FIFO_HALF_FULL_LSB 1
#define UART_ISR_RX_FIFO_HALF_FULL_MASK 0x2
#define UART_ISR_RX_FIFO_HALF_FULL_RESET 0x0

// ISR.TX_FIFO_EMPTY - Tx FIFO went from non-empty to empty state.
#define UART_ISR_TX_FIFO_EMPTY_WIDTH 1
#define UART_ISR_TX_FIFO_EMPTY_LSB 2
#define UART_ISR_TX_FIFO_EMPTY_MASK 0x4
#define UART_ISR_TX_FIFO_EMPTY_RESET 0x0

// ISR.TX_FIFO_HALF_EMPTY - Tx FIFO filling level dropped below the half empty threshold.
#define UART_ISR_TX_FIFO_HALF_EMPTY_WIDTH 1
#define UART_ISR_TX_FIFO_HALF_EMPTY_LSB 3
#define UART_ISR_TX_FIFO_HALF_EMPTY_MASK 0x8
#define UART_ISR_TX_FIFO_HALF_EMPTY_RESET 0x0

// IEN - Interrupt enable register
#define UART_IEN_ADDR 0x14
#define UART_IEN_RESET 0x0
typedef struct {
    uint32_t RX_DATA_AVL : 1; // Enable RX_DATA_AVL interrupt.
    uint32_t RX_FIFO_HALF_FULL : 1; // Enable RX_FIFO_HALF_FULL interrupt.
    uint32_t TX_FIFO_EMPTY : 1; // Enable TX_FIFO_EMPTY interrupt.
    uint32_t TX_FIFO_HALF_EMPTY : 1; // Enabled TX_FIFO_HALF_EMPTY interrupt.
    uint32_t : 28; // reserved
} uart_ien_t;

// IEN.RX_DATA_AVL - Enable RX_DATA_AVL interrupt.
#define UART_IEN_RX_DATA_AVL_WIDTH 1
#define UART_IEN_RX_DATA_AVL_LSB 0
#define UART_IEN_RX_DATA_AVL_MASK 0x1
#define UART_IEN_RX_DATA_AVL_RESET 0x0

// IEN.RX_FIFO_HALF_FULL - Enable RX_FIFO_HALF_FULL interrupt.
#define UART_IEN_RX_FIFO_HALF_FULL_WIDTH 1
#define UART_IEN_RX_FIFO_HALF_FULL_LSB 1
#define UART_IEN_RX_FIFO_HALF_FULL_MASK 0x2
#define UART_IEN_RX_FIFO_HALF_FULL_RESET 0x0

// IEN.TX_FIFO_EMPTY - Enable TX_FIFO_EMPTY interrupt.
#define UART_IEN_TX_FIFO_EMPTY_WIDTH 1
#define UART_IEN_TX_FIFO_EMPTY_LSB 2
#define UART_IEN_TX_FIFO_EMPTY_MASK 0x4
#define UART_IEN_TX_FIFO_EMPTY_RESET 0x0

// IEN.TX_FIFO_HALF_EMPTY - Enabled TX_FIFO_HALF_EMPTY interrupt.
#define UART_IEN_TX_FIFO_HALF_EMPTY_WIDTH 1
#define UART_IEN_TX_FIFO_HALF_EMPTY_LSB 3
#define UART_IEN_TX_FIFO_HALF_EMPTY_MASK 0x8
#define UART_IEN_TX_FIFO_HALF_EMPTY_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t SETUP; // Setup register
        __IO uart_setup_t SETUP_bf; // Bit access for SETUP register
    };
    union {
        __I uint32_t FIFO; // Rx and Tx FIFO size and status
        __I uart_fifo_t FIFO_bf; // Bit access for FIFO register
    };
    union {
        __IO uint32_t RXDATA; // Rx data register.
        __IO uart_rxdata_t RXDATA_bf; // Bit access for RXDATA register
    };
    union {
        __IO uint32_t TXDATA; // Tx data register.
        __IO uart_txdata_t TXDATA_bf; // Bit access for TXDATA register
    };
    union {
        __IO uint32_t ISR; // Interrupt status register
        __IO uart_isr_t ISR_bf; // Bit access for ISR register
    };
    union {
        __IO uint32_t IEN; // Interrupt enable register
        __IO uart_ien_t IEN_bf; // Bit access for IEN register
    };
} uart_t;

#define UART ((uart_t*)(UART_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __UART_REGS_H */
