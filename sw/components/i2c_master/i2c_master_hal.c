#include "i2c_master_hal.h"
#include "i2c_master_regs.h"
#include <assert.h>
#include <string.h>

static volatile char *i2c_master_buf_ptr = (volatile char*)(I2C_MASTER_BASE + I2C_MASTER_MEM_BASE);

void i2c_master_reset(void) {
  i2c_master_reg_wr(I2C_MASTER_CMD, I2C_MASTER_CMD_RST);
}

void i2c_master_set_speed(unsigned ticks_per_i2c_bit) {
  assert(ticks_per_i2c_bit <= I2C_MASTER_SPD_MASK);

  i2c_master_reg_wr(I2C_MASTER_SPD, ticks_per_i2c_bit);
}

unsigned i2c_master_get_speed(void) {
  return i2c_master_reg_rd(I2C_MASTER_SPD);
}

volatile char* i2c_master_get_buf_ptr(void) {
  return i2c_master_buf_ptr;
}

int i2c_master_err(void) {
  return i2c_master_reg_rd(I2C_MASTER_CMD) & I2C_MASTER_CMD_ERR;
}

int i2c_master_busy(void) {
  return i2c_master_reg_rd(I2C_MASTER_CMD) & I2C_MASTER_CMD_BUSY;
}

void i2c_master_write_buf_to_slave(unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes) {
  num_bytes &= I2C_MASTER_CMD_NUM_BYTES_MASK;
  slv_addr <<= I2C_MASTER_CMD_SLV_ADDR_OFFSET;
  slv_addr &= I2C_MASTER_CMD_SLV_ADDR_MASK;
  slv_reg_addr <<= I2C_MASTER_CMD_START_ADDR_OFFSET;
  slv_reg_addr &= I2C_MASTER_CMD_START_ADDR_MASK;

  i2c_master_reg_wr(I2C_MASTER_CMD, slv_addr | slv_reg_addr | num_bytes);
}

void i2c_master_read_slave_to_buf(unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes) {
  num_bytes &= I2C_MASTER_CMD_NUM_BYTES_MASK;
  slv_addr <<= I2C_MASTER_CMD_SLV_ADDR_OFFSET;
  slv_addr &= I2C_MASTER_CMD_SLV_ADDR_MASK;
  slv_reg_addr <<= I2C_MASTER_CMD_START_ADDR_OFFSET;
  slv_reg_addr &= I2C_MASTER_CMD_START_ADDR_MASK;

  i2c_master_reg_wr(I2C_MASTER_CMD, slv_addr | I2C_MASTER_CMD_RD | slv_reg_addr | num_bytes);
}

void i2c_master_write_bytes(char *bytes_to_write, unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes) {
  assert(slv_reg_addr + num_bytes <= I2C_MASTER_BUF_SIZE_BYTES);

  while (i2c_master_busy());

  memcpy((void*)(i2c_master_buf_ptr + slv_reg_addr), bytes_to_write, num_bytes);
  i2c_master_write_buf_to_slave(slv_addr, slv_reg_addr, num_bytes);
}

void i2c_master_read_bytes(char *rx_buf, unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes) {
  assert(slv_reg_addr + num_bytes <= I2C_MASTER_BUF_SIZE_BYTES);

  while (i2c_master_busy());

  i2c_master_read_slave_to_buf(slv_addr, slv_reg_addr, num_bytes);

  while (i2c_master_busy());

  memcpy(rx_buf, (void*)(i2c_master_buf_ptr + slv_reg_addr), num_bytes);
}

