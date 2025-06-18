// Created with Corsair v1.0.4
#ifndef __SDSPI_REGS_H
#define __SDSPI_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define SDSPI_BASE_ADDR 0x10000020

// CMD - Command and status register
#define SDSPI_CMD_ADDR 0x0
#define SDSPI_CMD_RESET 0x0
typedef struct {
    uint32_t R1_CMD : 8; // On write, if bits [7:6]==01 and card idle, remaining bits are sent to card. Contains R1 response when command has completed.
    uint32_t ERESP : 2; // Expected response.
    uint32_t WR : 1; // 1 to write from FIFO to card, 0 to read from card into FIFO. Assumes F is set.
    uint32_t F : 1; // Set if FIFO data transmision accompanies command.
    uint32_t SEL : 1; // 1 = select FIFO[0], 0 = select FIFO[1].
    uint32_t : 1; // reserved
    uint32_t BUSY : 1; // Busy bit, set while command is running.
    uint32_t ERR : 1; // Error indication. Time-out, card reset, CRC error, R1 response error.
    uint32_t : 2; // reserved
    uint32_t REM : 1; // Card has been removed since last read. If P=0 and R=1, card has been inserted and needs initialization.
    uint32_t P : 1; // 1 = card missing, 0 = card present.
    uint32_t : 12; // reserved
} sdspi_cmd_t;

// CMD.R1_CMD - On write, if bits [7:6]==01 and card idle, remaining bits are sent to card. Contains R1 response when command has completed.
#define SDSPI_CMD_R1_CMD_WIDTH 8
#define SDSPI_CMD_R1_CMD_LSB 0
#define SDSPI_CMD_R1_CMD_MASK 0xff
#define SDSPI_CMD_R1_CMD_RESET 0x0

// CMD.ERESP - Expected response.
#define SDSPI_CMD_ERESP_WIDTH 2
#define SDSPI_CMD_ERESP_LSB 8
#define SDSPI_CMD_ERESP_MASK 0x300
#define SDSPI_CMD_ERESP_RESET 0x0
typedef enum {
    SDSPI_CMD_ERESP_R1_RESP = 0x0, //Expect R1 response.
    SDSPI_CMD_ERESP_R1B_RESP = 0x1, //Expect R1b response.
    SDSPI_CMD_ERESP_R2_R3_R7 = 0x2, //Expect R2, R3, R7 32-bit response.
} sdspi_cmd_eresp_t;

// CMD.WR - 1 to write from FIFO to card, 0 to read from card into FIFO. Assumes F is set.
#define SDSPI_CMD_WR_WIDTH 1
#define SDSPI_CMD_WR_LSB 10
#define SDSPI_CMD_WR_MASK 0x400
#define SDSPI_CMD_WR_RESET 0x0

// CMD.F - Set if FIFO data transmision accompanies command.
#define SDSPI_CMD_F_WIDTH 1
#define SDSPI_CMD_F_LSB 11
#define SDSPI_CMD_F_MASK 0x800
#define SDSPI_CMD_F_RESET 0x0

// CMD.SEL - 1 = select FIFO[0], 0 = select FIFO[1].
#define SDSPI_CMD_SEL_WIDTH 1
#define SDSPI_CMD_SEL_LSB 12
#define SDSPI_CMD_SEL_MASK 0x1000
#define SDSPI_CMD_SEL_RESET 0x0

// CMD.BUSY - Busy bit, set while command is running.
#define SDSPI_CMD_BUSY_WIDTH 1
#define SDSPI_CMD_BUSY_LSB 14
#define SDSPI_CMD_BUSY_MASK 0x4000
#define SDSPI_CMD_BUSY_RESET 0x0

// CMD.ERR - Error indication. Time-out, card reset, CRC error, R1 response error.
#define SDSPI_CMD_ERR_WIDTH 1
#define SDSPI_CMD_ERR_LSB 15
#define SDSPI_CMD_ERR_MASK 0x8000
#define SDSPI_CMD_ERR_RESET 0x0

// CMD.REM - Card has been removed since last read. If P=0 and R=1, card has been inserted and needs initialization.
#define SDSPI_CMD_REM_WIDTH 1
#define SDSPI_CMD_REM_LSB 18
#define SDSPI_CMD_REM_MASK 0x40000
#define SDSPI_CMD_REM_RESET 0x0

// CMD.P - 1 = card missing, 0 = card present.
#define SDSPI_CMD_P_WIDTH 1
#define SDSPI_CMD_P_LSB 19
#define SDSPI_CMD_P_MASK 0x80000
#define SDSPI_CMD_P_RESET 0x0

// DAT - Return data/argument register
#define SDSPI_DAT_ADDR 0x4
#define SDSPI_DAT_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Command argument, command response. R2 response is in upper 8-bits.
} sdspi_dat_t;

// DAT.VALUE - Command argument, command response. R2 response is in upper 8-bits.
#define SDSPI_DAT_VALUE_WIDTH 32
#define SDSPI_DAT_VALUE_LSB 0
#define SDSPI_DAT_VALUE_MASK 0xffffffff
#define SDSPI_DAT_VALUE_RESET 0x0

// FIFO_0 - 128 word FIFO[0] data
#define SDSPI_FIFO_0_ADDR 0x8
#define SDSPI_FIFO_0_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Data read from or to write to card.
} sdspi_fifo_0_t;

// FIFO_0.VALUE - Data read from or to write to card.
#define SDSPI_FIFO_0_VALUE_WIDTH 32
#define SDSPI_FIFO_0_VALUE_LSB 0
#define SDSPI_FIFO_0_VALUE_MASK 0xffffffff
#define SDSPI_FIFO_0_VALUE_RESET 0x0

// FIFO_1 - 128 word FIFO[1] data
#define SDSPI_FIFO_1_ADDR 0xc
#define SDSPI_FIFO_1_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Data read from or to write to card.
} sdspi_fifo_1_t;

// FIFO_1.VALUE - Data read from or to write to card.
#define SDSPI_FIFO_1_VALUE_WIDTH 32
#define SDSPI_FIFO_1_VALUE_LSB 0
#define SDSPI_FIFO_1_VALUE_MASK 0xffffffff
#define SDSPI_FIFO_1_VALUE_RESET 0x0

// ISR - Interrupt status register
#define SDSPI_ISR_ADDR 0x10
#define SDSPI_ISR_RESET 0x0
typedef struct {
    uint32_t BUSY : 1; // Set when controller goes from busy to non-busy state
    uint32_t CARD_REMOVED : 1; // Set when controller detects that the SD card has been removed.
    uint32_t : 30; // reserved
} sdspi_isr_t;

// ISR.BUSY - Set when controller goes from busy to non-busy state
#define SDSPI_ISR_BUSY_WIDTH 1
#define SDSPI_ISR_BUSY_LSB 0
#define SDSPI_ISR_BUSY_MASK 0x1
#define SDSPI_ISR_BUSY_RESET 0x0

// ISR.CARD_REMOVED - Set when controller detects that the SD card has been removed.
#define SDSPI_ISR_CARD_REMOVED_WIDTH 1
#define SDSPI_ISR_CARD_REMOVED_LSB 1
#define SDSPI_ISR_CARD_REMOVED_MASK 0x2
#define SDSPI_ISR_CARD_REMOVED_RESET 0x0

// IEN - Interrupt enable register
#define SDSPI_IEN_ADDR 0x14
#define SDSPI_IEN_RESET 0x0
typedef struct {
    uint32_t BUSY : 1; // Set to enable BUSY interrupt.
    uint32_t CARD_REMOVED : 1; // Set to enabled CARD_REMOVED interrupt.
    uint32_t : 30; // reserved
} sdspi_ien_t;

// IEN.BUSY - Set to enable BUSY interrupt.
#define SDSPI_IEN_BUSY_WIDTH 1
#define SDSPI_IEN_BUSY_LSB 0
#define SDSPI_IEN_BUSY_MASK 0x1
#define SDSPI_IEN_BUSY_RESET 0x0

// IEN.CARD_REMOVED - Set to enabled CARD_REMOVED interrupt.
#define SDSPI_IEN_CARD_REMOVED_WIDTH 1
#define SDSPI_IEN_CARD_REMOVED_LSB 1
#define SDSPI_IEN_CARD_REMOVED_MASK 0x2
#define SDSPI_IEN_CARD_REMOVED_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t CMD; // Command and status register
        __IO sdspi_cmd_t CMD_bf; // Bit access for CMD register
    };
    union {
        __IO uint32_t DAT; // Return data/argument register
        __IO sdspi_dat_t DAT_bf; // Bit access for DAT register
    };
    union {
        __IO uint32_t FIFO_0; // 128 word FIFO[0] data
        __IO sdspi_fifo_0_t FIFO_0_bf; // Bit access for FIFO_0 register
    };
    union {
        __IO uint32_t FIFO_1; // 128 word FIFO[1] data
        __IO sdspi_fifo_1_t FIFO_1_bf; // Bit access for FIFO_1 register
    };
    union {
        __IO uint32_t ISR; // Interrupt status register
        __IO sdspi_isr_t ISR_bf; // Bit access for ISR register
    };
    union {
        __IO uint32_t IEN; // Interrupt enable register
        __IO sdspi_ien_t IEN_bf; // Bit access for IEN register
    };
} sdspi_t;

#define SDSPI ((sdspi_t*)(SDSPI_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __SDSPI_REGS_H */
