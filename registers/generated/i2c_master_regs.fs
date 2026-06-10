\ --- I2C_MASTER

$10000200 constant I2C_MASTER_BASE_ADDR

\ CMD - I2C master command register
$0 constant I2C_MASTER_CMD_ADDR

\ CMD.NUM_BYTES - Number of bytes to read/write.
8 constant I2C_MASTER_CMD_NUM_BYTES_WIDTH
0 constant I2C_MASTER_CMD_NUM_BYTES_LSB
$ff constant I2C_MASTER_CMD_NUM_BYTES_MASK
\ CMD.START_ADDR - Initial address to read from or write to.
8 constant I2C_MASTER_CMD_START_ADDR_WIDTH
8 constant I2C_MASTER_CMD_START_ADDR_LSB
$ff00 constant I2C_MASTER_CMD_START_ADDR_MASK
\ CMD.RD_N_WR - Set to 1 for a read command, 0 for a write command.
1 constant I2C_MASTER_CMD_RD_N_WR_WIDTH
16 constant I2C_MASTER_CMD_RD_N_WR_LSB
$10000 constant I2C_MASTER_CMD_RD_N_WR_MASK
\ CMD.SLAVE_ADDR - I2C slave address
7 constant I2C_MASTER_CMD_SLAVE_ADDR_WIDTH
17 constant I2C_MASTER_CMD_SLAVE_ADDR_LSB
$fe0000 constant I2C_MASTER_CMD_SLAVE_ADDR_MASK
\ CMD.ERR - Command error
1 constant I2C_MASTER_CMD_ERR_WIDTH
30 constant I2C_MASTER_CMD_ERR_LSB
$40000000 constant I2C_MASTER_CMD_ERR_MASK
\ CMD.BUSY - Transaction ongoing.
1 constant I2C_MASTER_CMD_BUSY_WIDTH
31 constant I2C_MASTER_CMD_BUSY_LSB
$80000000 constant I2C_MASTER_CMD_BUSY_MASK
\ SPD - Speed register
$4 constant I2C_MASTER_SPD_ADDR

\ SPD.VALUE - Number of system clocks for I2C wait state (1/4th of I2C bus clock period).
20 constant I2C_MASTER_SPD_VALUE_WIDTH
0 constant I2C_MASTER_SPD_VALUE_LSB
$fffff constant I2C_MASTER_SPD_VALUE_MASK
\ ISR - Interrupt Status Register
$8 constant I2C_MASTER_ISR_ADDR

\ ISR.BUSY - Set when I2C goes from busy to idle.
1 constant I2C_MASTER_ISR_BUSY_WIDTH
0 constant I2C_MASTER_ISR_BUSY_LSB
$1 constant I2C_MASTER_ISR_BUSY_MASK
\ IEN - Interrupt Enable Register
$c constant I2C_MASTER_IEN_ADDR

\ IEN.BUSY - Set to enable BUSY interrupt.
1 constant I2C_MASTER_IEN_BUSY_WIDTH
0 constant I2C_MASTER_IEN_BUSY_LSB
$1 constant I2C_MASTER_IEN_BUSY_MASK
