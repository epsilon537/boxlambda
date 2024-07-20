#ifndef I2C_MASTER_REGS_H
#define I2C_MASTER_REGS_H

#include "stdint.h"

#define I2C_MASTER_BASE 0x10000200

//I2C Master Registers
#define I2C_MASTER_CMD 0
#define I2C_MASTER_CMD_BUSY 0x80000000
#define I2C_MASTER_CMD_ERR  0x40000000
#define I2C_MASTER_CMD_SLV_ADDR_OFFSET 17
#define I2C_MASTER_CMD_SLV_ADDR_MASK 0x00fe0000
#define I2C_MASTER_CMD_RD   0x00010000
#define I2C_MASTER_CMD_WR   0x00000000
#define I2C_MASTER_CMD_START_ADDR_OFFSET 8
#define I2C_MASTER_CMD_START_ADDR_MASK 0x0000ff00 /*Initial address to read from or write to.*/
#define I2C_MASTER_CMD_NUM_BYTES_OFFSET 0
#define I2C_MASTER_CMD_NUM_BYTES_MASK 0xff /*Number of bytes to read/write*/

// r_speed ... the programmable number of system clocks per I2C
// wait state.  Nominally, this is one quarter the clock period of the
// I2C bus.
#define I2C_MASTER_SPD 4
#define I2C_MASTER_SPD_MASK 0xfffff /*Max. 20 bits*/

#define I2C_MASTER_MEM_BASE 256 /*Address offset of the 128 byte memory buffer*/
#define I2C_MASTER_MEM_SIZE_BYTES 256

inline void i2c_master_reg_wr(uint32_t reg_offset, uint32_t data) {
  *(volatile uint32_t *)(I2C_MASTER_BASE + reg_offset) = data;
}

inline uint32_t i2c_master_reg_rd(uint32_t reg_offset) {
  uint32_t res = *(uint32_t volatile *)(I2C_MASTER_BASE + reg_offset);
  return res;
}

#endif /*I2C_MASTER_REGS_H*/
