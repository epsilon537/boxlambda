// Created with Corsair v1.0.4
#ifndef __I2C_MASTER_REGS_H
#define __I2C_MASTER_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define I2C_MASTER_BASE_ADDR 0x10000200

// CMD - I2C master command register
#define I2C_MASTER_CMD_ADDR 0x0
#define I2C_MASTER_CMD_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t NUM_BYTES : 8; // Number of bytes to read/write.
    uint32_t START_ADDR : 8; // Initial address to read from or write to.
    uint32_t RD_N_WR : 1; // Set to 1 for a read command, 0 for a write command.
    uint32_t SLAVE_ADDR : 7; // I2C slave address
    uint32_t : 6; // reserved
    uint32_t ERR : 1; // Command error
    uint32_t BUSY : 1; // Transaction ongoing.
  };
} i2c_master_cmd_t;

// CMD.NUM_BYTES - Number of bytes to read/write.
#define I2C_MASTER_CMD_NUM_BYTES_WIDTH 8
#define I2C_MASTER_CMD_NUM_BYTES_LSB 0
#define I2C_MASTER_CMD_NUM_BYTES_MASK 0xff
#define I2C_MASTER_CMD_NUM_BYTES_RESET 0x0

// CMD.START_ADDR - Initial address to read from or write to.
#define I2C_MASTER_CMD_START_ADDR_WIDTH 8
#define I2C_MASTER_CMD_START_ADDR_LSB 8
#define I2C_MASTER_CMD_START_ADDR_MASK 0xff00
#define I2C_MASTER_CMD_START_ADDR_RESET 0x0

// CMD.RD_N_WR - Set to 1 for a read command, 0 for a write command.
#define I2C_MASTER_CMD_RD_N_WR_WIDTH 1
#define I2C_MASTER_CMD_RD_N_WR_LSB 16
#define I2C_MASTER_CMD_RD_N_WR_MASK 0x10000
#define I2C_MASTER_CMD_RD_N_WR_RESET 0x0

// CMD.SLAVE_ADDR - I2C slave address
#define I2C_MASTER_CMD_SLAVE_ADDR_WIDTH 7
#define I2C_MASTER_CMD_SLAVE_ADDR_LSB 17
#define I2C_MASTER_CMD_SLAVE_ADDR_MASK 0xfe0000
#define I2C_MASTER_CMD_SLAVE_ADDR_RESET 0x0

// CMD.ERR - Command error
#define I2C_MASTER_CMD_ERR_WIDTH 1
#define I2C_MASTER_CMD_ERR_LSB 30
#define I2C_MASTER_CMD_ERR_MASK 0x40000000
#define I2C_MASTER_CMD_ERR_RESET 0x0

// CMD.BUSY - Transaction ongoing.
#define I2C_MASTER_CMD_BUSY_WIDTH 1
#define I2C_MASTER_CMD_BUSY_LSB 31
#define I2C_MASTER_CMD_BUSY_MASK 0x80000000
#define I2C_MASTER_CMD_BUSY_RESET 0x0

// SPD - Speed register
#define I2C_MASTER_SPD_ADDR 0x4
#define I2C_MASTER_SPD_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 20; // Number of system clocks for I2C wait state (1/4th of I2C bus clock period).
    uint32_t : 12; // reserved
  };
} i2c_master_spd_t;

// SPD.VALUE - Number of system clocks for I2C wait state (1/4th of I2C bus clock period).
#define I2C_MASTER_SPD_VALUE_WIDTH 20
#define I2C_MASTER_SPD_VALUE_LSB 0
#define I2C_MASTER_SPD_VALUE_MASK 0xfffff
#define I2C_MASTER_SPD_VALUE_RESET 0x0

// ISR - Interrupt Status Register
#define I2C_MASTER_ISR_ADDR 0x8
#define I2C_MASTER_ISR_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t BUSY : 1; // Set when I2C goes from busy to idle.
    uint32_t : 31; // reserved
  };
} i2c_master_isr_t;

// ISR.BUSY - Set when I2C goes from busy to idle.
#define I2C_MASTER_ISR_BUSY_WIDTH 1
#define I2C_MASTER_ISR_BUSY_LSB 0
#define I2C_MASTER_ISR_BUSY_MASK 0x1
#define I2C_MASTER_ISR_BUSY_RESET 0x0

// IEN - Interrupt Enable Register
#define I2C_MASTER_IEN_ADDR 0xc
#define I2C_MASTER_IEN_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t BUSY : 1; // Set to enable BUSY interrupt.
    uint32_t : 31; // reserved
  };
} i2c_master_ien_t;

// IEN.BUSY - Set to enable BUSY interrupt.
#define I2C_MASTER_IEN_BUSY_WIDTH 1
#define I2C_MASTER_IEN_BUSY_LSB 0
#define I2C_MASTER_IEN_BUSY_MASK 0x1
#define I2C_MASTER_IEN_BUSY_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t CMD; // I2C master command register
        __IO i2c_master_cmd_t CMD_bf; // Bit access for CMD register
    };
    union {
        __IO uint32_t SPD; // Speed register
        __IO i2c_master_spd_t SPD_bf; // Bit access for SPD register
    };
    union {
        __IO uint32_t ISR; // Interrupt Status Register
        __IO i2c_master_isr_t ISR_bf; // Bit access for ISR register
    };
    union {
        __IO uint32_t IEN; // Interrupt Enable Register
        __IO i2c_master_ien_t IEN_bf; // Bit access for IEN register
    };
} i2c_master_t;

#define I2C_MASTER ((i2c_master_t*)(I2C_MASTER_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __I2C_MASTER_REGS_H */
