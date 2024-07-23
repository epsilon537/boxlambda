#include "i2c.h"
#include "i2c_regs.h"
#include "mcycle.h"
#include <assert.h>
#include <string.h>

#define SLAVE_ADDR_UNASSIGNED (~0UL)
#define BUF_IDX_UNKNOWN (~0UL)

//Default: 125*4 50MHz sysclock ticks per I2C clock -> 100kHz.
#define I2C_SPEED_DEFAULT 125

//Singleton
I2C i2c;

I2C::I2C() : i2c_master_buf_ptr_((volatile uint8_t*)(I2C_MASTER_BASE + I2C_MASTER_MEM_BASE)), slaveAddr_(SLAVE_ADDR_UNASSIGNED), bufStartIdx_(BUF_IDX_UNKNOWN), readIdx_(BUF_IDX_UNKNOWN), numBytes_(0) {}

bool I2C::isBusy_() {
  return (i2c_master_reg_rd(I2C_MASTER_CMD) & (I2C_MASTER_CMD_BUSY | I2C_MASTER_CMD_NUM_BYTES_MASK)) != 0;
}

bool I2C::i2cError_() {
  return (i2c_master_reg_rd(I2C_MASTER_CMD) & I2C_MASTER_CMD_ERR) != 0;
}

void I2C::begin() {
  //Set default bus speed.
  i2c_master_reg_wr(I2C_MASTER_SPD, I2C_SPEED_DEFAULT);
  slaveAddr_ = SLAVE_ADDR_UNASSIGNED;
  bufStartIdx_ = BUF_IDX_UNKNOWN;
  readIdx_ = BUF_IDX_UNKNOWN;
  numBytes_ = 0;
}

void I2C::enableIRQ(bool enable) {
  i2c_master_reg_wr(I2C_IEN, enable ? I2C_IEN_BUSY : 0);
}

void I2C::ackIRQ() {
  i2c_master_reg_wr(I2C_ISR, I2C_ISR_BUSY);
}

void I2C::setClock(uint32_t clockSpeedHz) {
  uint32_t ticks_per_i2c_bit = PLATFORM_CLK_FREQ / (4*clockSpeedHz);

  assert(ticks_per_i2c_bit <= I2C_MASTER_SPD_MASK);
  i2c_master_reg_wr(I2C_MASTER_SPD, ticks_per_i2c_bit);
}

void I2C::beginTransmission(uint8_t slaveAddr) {
  slaveAddr_ = (uint32_t)slaveAddr;
  bufStartIdx_ = BUF_IDX_UNKNOWN;
  numBytes_ = 0;
}

uint8_t I2C::endTransmission() {
  uint8_t res = 0;

  assert(slaveAddr_ != SLAVE_ADDR_UNASSIGNED);
  assert(bufStartIdx_ != BUF_IDX_UNKNOWN);

  /* This is hacky, but necessary to match this API to the wbi2c core logic:
   * If we dont't have any bytes written yet when endTransmission() is called, we're dealing
   * with a read-from-slave operation.
   * We don't know yet how many bytes the caller would like to read from the slave. We have
   * to wait for a requestFrom() call to get the number of bytes to read. In other words,
   * if numBytes_ is 0, we do nothing.
   */
  if (numBytes_ > 0) {
    //I2C write transaction:
    uint32_t cmdReg = numBytes_ & I2C_MASTER_CMD_NUM_BYTES_MASK;
    cmdReg |= (slaveAddr_ << I2C_MASTER_CMD_SLV_ADDR_OFFSET) & I2C_MASTER_CMD_SLV_ADDR_MASK;
    cmdReg |= (bufStartIdx_ << I2C_MASTER_CMD_START_ADDR_OFFSET) & I2C_MASTER_CMD_START_ADDR_MASK;
    cmdReg |= I2C_MASTER_CMD_WR;

    /*Kick off write transaction to slave.*/
    i2c_master_reg_wr(I2C_MASTER_CMD, cmdReg);

    while (isBusy_());

    /* Clear the I2C local buffer, to make sure that a read from slave actually reads from
     * slave instead of returning stale buffer data.
     */
    memset((void*)(i2c_master_buf_ptr_ + bufStartIdx_), 0, numBytes_);

    slaveAddr_ = SLAVE_ADDR_UNASSIGNED;

    res = i2cError_() ? 1 : 0;
  }

  return res;
}

uint8_t I2C::requestFrom(uint8_t slaveAddr, uint8_t numBytes) {
  assert(bufStartIdx_ != BUF_IDX_UNKNOWN);
  assert(slaveAddr == slaveAddr_);
  assert(numBytes_ == 0);
  assert(numBytes > 0);

  numBytes_ = numBytes;

  //I2C read transaction
  uint32_t cmdReg = numBytes_ & I2C_MASTER_CMD_NUM_BYTES_MASK;
  cmdReg |= (slaveAddr_ << I2C_MASTER_CMD_SLV_ADDR_OFFSET) & I2C_MASTER_CMD_SLV_ADDR_MASK;
  cmdReg |= (bufStartIdx_ << I2C_MASTER_CMD_START_ADDR_OFFSET) & I2C_MASTER_CMD_START_ADDR_MASK;
  cmdReg |= I2C_MASTER_CMD_RD;

  /*Kick off read transaction to slave.*/
  i2c_master_reg_wr(I2C_MASTER_CMD, cmdReg);

  while (isBusy_());

  readIdx_ = bufStartIdx_;
  slaveAddr_ = SLAVE_ADDR_UNASSIGNED;

  return i2cError_() ? 1 : 0;
}

uint8_t I2C::read(void) {
  assert(bufStartIdx_ != BUF_IDX_UNKNOWN);
  assert(numBytes_ > 0);

  if (readIdx_ == numBytes_+bufStartIdx_) {
    //Wrap around.
    readIdx_ = bufStartIdx_;
  }

  return i2c_master_buf_ptr_[readIdx_++];
}

uint8_t I2C::write(uint8_t b) {
  assert(slaveAddr_ != SLAVE_ADDR_UNASSIGNED);

  /*First write is the slave reg address, which is the same of bufStartIdx_*/
  if (bufStartIdx_ == BUF_IDX_UNKNOWN) {
    bufStartIdx_ = (uint32_t)b;
    return 1;
  }

  if (bufStartIdx_+numBytes_ < I2C_MASTER_MEM_SIZE_BYTES) {
    i2c_master_buf_ptr_[bufStartIdx_+numBytes_] = b;
    ++numBytes_;
    return 1;
  }

  return 0;
}

