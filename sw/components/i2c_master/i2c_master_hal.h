#ifndef I2C_MASTER_HAL_H
#define I2C_MASTER_HAL_H

#define MAX_TICKS_PER_I2C_BIT 0xfffff
#define I2C_MASTER_BUF_SIZE_BYTES 128
#define I2C_MASTER_MAX_NUM_BYTES I2C_MASTER_BUF_SIZE_BYTES
#define I2C_SLAVE_ADDR_MAX 127
#define I2C_SLAVE_REG_ADDR_MAX 127

/*Reset the interface and abandon pending transaction.*/
void i2c_master_reset(void);

/*Set I2C bus speed in 50MHz system clock ticks per I2C bit. Max. MAX_TICKS_PER_I2C_BIT.*/
void i2c_master_set_speed(unsigned ticks_per_i2c_bit);

unsigned i2c_master_get_speed(void);

/* Get pointer to I2C master 128 byte buffer. The two functions below write from this buffer to the I2C slave
 * and read from the I2C slave into this buffer.
 */
volatile char* i2c_master_get_buf_ptr(void);

/* Returns non-zero if previous transaction reported an error.
 */
int i2c_master_err(void);

/* Returns non-zero if a transaction is ongoing, 0 if idle.
 */
int i2c_master_busy(void);

/* Write contents of I2C master buffer to slave. Does not check master state before starting and does not block. Use i2c_master_busy().
 * slv_addr: 0-I2C_SLAVE_ADDR_MAX: I2C slave device address
 * slv_reg_addr:  0-I2C_SLAVE_REG_ADDR_MAX: I2C slave register start address
 * num_bytes: 0-I2C_MASTER_MAX_NUM_BYTES
 */
void i2c_master_write_buf_to_slave(unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes);

/* Read byte sequence from slave into I2C Master buffer. Does not check master state before starting and does not block. Use i2c_master_busy().
 * slv_addr: 0-I2C_SLAVE_ADDR_MAX: I2C slave device address
 * slv_reg_addr:  0-I2C_SLAVE_REG_ADDR_MAX: I2C slave register start address
 * num_bytes: 0-I2C_MASTER_MAX_NUM_BYTES
 */
void i2c_master_read_slave_to_buf(unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes);

/* Write byte sequence to slave. Combines writing to I2C Master buffer and transferring to slave.
 * Will check/wait for I2C bus idle state before starting. Will not wait for I2C bus idle before returning. Use i2c_master_busy() if needed.
 * slv_addr: 0-I2C_SLAVE_ADDR_MAX: I2C slave device address
 * slv_reg_addr:  0-I2C_SLAVE_REG_ADDR_MAX: I2C slave register start address
 * num_bytes: 0-I2C_MASTER_MAX_NUM_BYTES
 */
void i2c_master_write_bytes(char *bytes_to_write, unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes);

/* Read byte sequence from slave. Combines transferring from slave and reading from I2C master buffer.
 * Will check/wait for I2C bus idle state before starting and ending.
 * slv_addr: 0-I2C_SLAVE_ADDR_MAX: I2C slave device address
 * slv_reg_addr:  0-I2C_SLAVE_REG_ADDR_MAX: I2C slave register start address
 * num_bytes: 0-I2C_MASTER_MAX_NUM_BYTES
 */
void i2c_master_read_bytes(char *rx_buf, unsigned slv_addr, unsigned slv_reg_addr, unsigned num_bytes);

#endif /*I2C_MASTER_HAL_H*/

