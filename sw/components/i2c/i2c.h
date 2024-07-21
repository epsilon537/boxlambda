#ifndef I2C_H
#define I2C_H

#include "stdint.h"

/*This I2C API is loosely based on Arduino's Wire API.*/

class I2C {
  private:
    uint32_t slaveAddr_;
    uint32_t bufStartIdx_;
    uint32_t readIdx_;
    uint32_t numBytes_;
    volatile uint8_t *i2c_master_buf_ptr_;

    bool isBusy_();
    bool i2cError_();

  public:
    I2C();

    /*!
     * @brief Initialize library for main function
     */
    void begin();

    /*
     * @brief Enable interrupt generation when bus goes from busy to idle
     */
    void enableIRQ(bool enable);

    /*
     * @brief Acknowledge IRQ
     */
    void ackIRQ();

    /*!
     * @brief Set I2C bus clock speed. Default value (i.e. the bus clock frequency
     * used if you don't call this function): 100000Hz.
     * @param clockSpeedHz I2C bus clock frequency in Hz. Minimum value: 50Hz.
     */
    void setClock(uint32_t clockSpeedHz);

    /*!
     * @brief Start recording a new transaction.
     * @param slaveAddr Secondary device address
     */
    void beginTransmission(uint8_t slaveAddr);

    /*!
     * @brief Ends the transmission
     * @return Returns the error code if there was one, 0 if successful.
     */
    uint8_t endTransmission();

    /*!
     * @brief Setup for receiving from secondary
     * @param slaveAddr Secondary device address
     * @param numBytes How many bytes to request
     * @return Returns 0, if there was an error, returns the error code
     */
    uint8_t requestFrom(uint8_t slaveAddr, uint8_t numBytes);

    /*!
     * @brief Returns bytes received (via requestFrom) one at a time.
     * @return Returns the data
     */
    uint8_t read(void);

    /*!
     * @brief Buffers up one byte of data to send
     * @param b Data to send
     * @return Returns 1 when successful
     */
    uint8_t write(uint8_t b);
};

extern I2C i2c;

#endif /*I2C_H*/
