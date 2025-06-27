#include "i2c.h"
#include "i2c_master_regs.h"
#include "mcycle.h"
#include <assert.h>
#include <string.h>

#define I2C_MASTER_MEM_BASE 256 /*Address offset of the 256 byte memory buffer*/
#define I2C_MASTER_MEM_SIZE_BYTES 256

#define SLAVE_ADDR_UNASSIGNED (~0UL)
#define BUF_IDX_UNKNOWN (~0UL)

//Default: 125*4 50MHz sysclock ticks per I2C clock -> 100kHz.
#define I2C_SPEED_DEFAULT 125

//Singleton
I2C i2c;

I2C::I2C() : i2c_mem_((volatile uint8_t*)(I2C_MASTER_BASE_ADDR + I2C_MASTER_MEM_BASE)), slaveAddr_(SLAVE_ADDR_UNASSIGNED), bufStartIdx_(BUF_IDX_UNKNOWN), readIdx_(BUF_IDX_UNKNOWN), numBytes_(0) {}

bool I2C::isBusy_() {
  return (I2C_MASTER->CMD & (I2C_MASTER_CMD_BUSY_MASK | I2C_MASTER_CMD_NUM_BYTES_MASK)) != 0;
}

bool I2C::i2cError_() {
  return (I2C_MASTER->CMD_bf.ERR != 0);
}

void I2C::begin() {
  //Set default bus speed.
  I2C_MASTER->SPD = I2C_SPEED_DEFAULT;
  slaveAddr_ = SLAVE_ADDR_UNASSIGNED;
  bufStartIdx_ = BUF_IDX_UNKNOWN;
  readIdx_ = BUF_IDX_UNKNOWN;
  numBytes_ = 0;
}

void I2C::enableIRQ(bool enable) {
  I2C_MASTER->IEN_bf.BUSY = enable ? 1 : 0;
}

void I2C::ackIRQ() {
  I2C_MASTER->ISR_bf.BUSY = 1;
}

void I2C::setClock(uint32_t clockSpeedHz) {
  uint32_t ticks_per_i2c_bit = PLATFORM_CLK_FREQ / (4*clockSpeedHz);

  assert(ticks_per_i2c_bit <= I2C_MASTER_SPD_VALUE_MASK);
  I2C_MASTER->SPD = ticks_per_i2c_bit;
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
    //I2C write transaction.
    i2c_master_cmd_t cmd{.NUM_BYTES=numBytes_, .START_ADDR=bufStartIdx_, .RD_N_WR=0, .SLAVE_ADDR=slaveAddr_};
    //Write as a 32-bit word to avoid field-by-field writes
    I2C_MASTER->CMD = *(uint32_t*)&cmd;

    while (isBusy_());

    /* Clear the I2C local buffer, to make sure that a read from slave actually reads from
     * slave instead of returning stale buffer data.
     */
    for (int ii=0;ii<numBytes_;ii++) {
      i2c_mem_[(bufStartIdx_+ii) & 0xff] = 0;
    }

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

  //I2C read transaction - apologies for the ugly cast.
  i2c_master_cmd_t cmd{.NUM_BYTES=numBytes_, .START_ADDR=bufStartIdx_, .RD_N_WR=1, .SLAVE_ADDR=slaveAddr_};
  //Write as a 32-bit word to avoid field-by-field writes
  I2C_MASTER->CMD = *(uint32_t*)&cmd;

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

  return i2c_mem_[(readIdx_++)&0xff];
}

uint8_t I2C::write(uint8_t b) {
  assert(slaveAddr_ != SLAVE_ADDR_UNASSIGNED);

  /*First write is the slave reg address, which is the same of bufStartIdx_*/
  if (bufStartIdx_ == BUF_IDX_UNKNOWN) {
    bufStartIdx_ = (uint32_t)b;
    return 1;
  }

  if (bufStartIdx_+numBytes_ < I2C_MASTER_MEM_SIZE_BYTES) {
    i2c_mem_[(bufStartIdx_+numBytes_) & 0xff] = b;
    ++numBytes_;
    return 1;
  }

  return 0;
}

