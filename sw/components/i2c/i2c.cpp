#include "i2c.h"
#include "i2c_regs.h"
#include "mcycle.h"
#include <assert.h>
#include <string.h>

#define SLAVE_ADDR_UNASSIGNED (~0UL)
#define BUF_IDX_UNKNOWN (~0UL)

//Default: 125*4 50MHz sysclock ticks per I2C clock -> 100kHz.
#define I2C_SPEED_DEFAULT 125

I2C i2c;

I2C::I2C() : i2c_master_buf_ptr_((volatile uint8_t*)(I2C_MASTER_BASE + I2C_MASTER_MEM_BASE)) {}

bool I2C::isBusy_() {
  return (i2c_master_reg_rd(I2C_MASTER_CMD) & (I2C_MASTER_CMD_BUSY | I2C_MASTER_CMD_NUM_BYTES_MASK)) != 0;
}

bool I2C::i2cError_() {
  return (i2c_master_reg_rd(I2C_MASTER_CMD) & I2C_MASTER_CMD_ERR) != 0;
}

/*!
 * @brief Initialize library for main function
 */
void I2C::begin() {
  i2c_master_reg_wr(I2C_MASTER_SPD, I2C_SPEED_DEFAULT);
  slaveAddr_ = SLAVE_ADDR_UNASSIGNED;
}

void I2C::setClock(uint32_t clockSpeedHz) {
  uint32_t ticks_per_i2c_bit = PLATFORM_CLK_FREQ / (4*clockSpeedHz);

  assert(ticks_per_i2c_bit <= I2C_MASTER_SPD_MASK);
  i2c_master_reg_wr(I2C_MASTER_SPD, ticks_per_i2c_bit);
}

/*!
 * @brief Setup address & write bit
 * @param slaveAddr Secondary device address
 */
void I2C::beginTransmission(uint8_t slaveAddr) {
  assert(slaveAddr_ == SLAVE_ADDR_UNASSIGNED);

  slaveAddr_ = (uint32_t)slaveAddr;
  bufStartIdx_ = BUF_IDX_UNKNOWN;
  numBytes_ = 0;
}

/*!
 * @brief Ends the transmission
 * @return Returns the error code if there was one
 */
uint8_t I2C::endTransmission() {
  assert(slaveAddr_ != SLAVE_ADDR_UNASSIGNED);
  assert(bufStartIdx_ != BUF_IDX_UNKNOWN);

  /* This is hacky, but necessary to match this API to the wbi2c core logic:
   * If we dont't have any bytes written yet when endTransmission() is called, we're dealing
   * with a read-from-slave operation.
   * We don't know yet how many bytes the caller would like to read from the slave. We have
   * to wait for a requestFrom() call to get the number of bytes to read.
   */
  if (numBytes_ > 0) {
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
    //memset((void*)(i2c_master_buf_ptr_ + bufStartIdx_), 0, numBytes_);

    slaveAddr_ = SLAVE_ADDR_UNASSIGNED;
  }

  return i2cError_() ? 1 : 0;
}

/*!
 * @brief Setup for receiving from secondary
 * @param slaveAddr Secondary device address
 * @param numBytes How many bytes to request
 * @return Returns 0, if there was an error, returns the error code
 */
uint8_t I2C::requestFrom(uint8_t slaveAddr, uint8_t numBytes) {
  assert(bufStartIdx_ != BUF_IDX_UNKNOWN);
  assert(slaveAddr == slaveAddr_);
  assert(numBytes_ == 0);
  assert(numBytes > 0);

  numBytes_ = numBytes;

  uint32_t cmdReg = numBytes_ & I2C_MASTER_CMD_NUM_BYTES_MASK;
  cmdReg |= (slaveAddr_ << I2C_MASTER_CMD_SLV_ADDR_OFFSET) & I2C_MASTER_CMD_SLV_ADDR_MASK;
  cmdReg |= (bufStartIdx_ << I2C_MASTER_CMD_START_ADDR_OFFSET) & I2C_MASTER_CMD_START_ADDR_MASK;
  cmdReg |= I2C_MASTER_CMD_RD;

  /*Kick off write transaction to slave.*/
  i2c_master_reg_wr(I2C_MASTER_CMD, cmdReg);

  while (isBusy_());

  readIdx_ = bufStartIdx_;
  slaveAddr_ = SLAVE_ADDR_UNASSIGNED;

  return i2cError_() ? 1 : 0;
}

/*!
 * @brief Receives data from the device
 * @return Returns the data
 */
uint8_t I2C::read(void) {
  assert(readIdx_ - bufStartIdx_ < numBytes_);
  return i2c_master_buf_ptr_[readIdx_++];
}

/*!
 * @brief Buffers up data to send
 * @param b Data to send
 * @return Returns 1 when successful
 */
uint8_t I2C::write(uint8_t b) {
  assert(slaveAddr_ != SLAVE_ADDR_UNASSIGNED);

  /*First write is the slave reg address, which is the same of bufStartIdx_*/
  if (bufStartIdx_ == BUF_IDX_UNKNOWN) {
    bufStartIdx_ = (uint32_t)b;
  }
  else {
    assert(numBytes_ < I2C_MASTER_MEM_SIZE_BYTES);

    i2c_master_buf_ptr_[bufStartIdx_+numBytes_] = b;
    ++numBytes_;
  }

  return 1;
}

