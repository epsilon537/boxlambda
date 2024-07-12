#ifndef I2C_MASTER_REGS_H
#define I2C_MASTER_REGS_H

#define I2C_MASTER_BASE 0x10000200

//I2C Master Registers
#define I2C_MASTER_CMD 0
#define I2C_MASTER_CMD_BUSY 0x80000000
#define I2C_MASTER_CMD_ERR  0x40000000
#define I2C_MASTER_CMD_SLV_ADDR_OFFSET 17
#define I2C_MASTER_CMD_SLV_ADDR_MASK 0x00fe0000
#define I2C_MASTER_CMD_RD   0x00010000
#define I2C_MASTER_CMD_WR   0x00000000
#define I2C_MASTER_CMD_RST  0x00008000 /*Reset interface and abandon pending transaction*/
#define I2C_MASTER_CMD_START_ADDR_OFFSET 8
#define I2C_MASTER_CMD_START_ADDR_MASK 0x00007f00 /*Initial address to read from or write to.*/
#define I2C_MASTER_CMD_NUM_BYTES_OFFSET 0
#define I2C_MASTER_CMD_NUM_BYTES_MASK 0x7f /*Number of bytes to read/write*/

#define I2C_MASTER_SPD 4 /*Number of 50MHz system clocks per I2C bit*/
#define I2C_MASTER_SPD_MASK 0xfffff /*Max. 20 bits*/

#define I2C_MASTER_MEM_BASE 128 /*Address offset of the 128 byte memory buffer*/
#define I2C_MASTER_MEM_SIZE_BYTES 128

inline void i2c_master_reg_wr(unsigned reg_offset, unsigned data) {
  *(volatile unsigned *)(I2C_MASTER_BASE + reg_offset) = data;
}

inline unsigned i2c_master_reg_rd(unsigned reg_offset) {
  return *(unsigned volatile *)(I2C_MASTER_BASE + reg_offset);
}

#endif /*I2C_MASTER_REGS_H*/
