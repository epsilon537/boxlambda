# I2C API

- **Integrated in OS**: No

- **Exported to Forth**: No

- **I2C Software Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/i2c](https://github.com/epsilon537/boxlambda/tree/master/sw/components/i2c)
- **I2C API**:
  [boxlambda/sw/components/i2c/i2c.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/i2c/i2c.h)

BoxLambda's I2C component is based on the [TinyWireM](https://github.com/adafruit/TinyWireM) API, which in turn is derived from Arduino's [Wire](https://www.arduino.cc/reference/en/language/functions/communication/wire/) API. This API makes it easy to port I2C-dependent components, such as the MCP79412RTC library, discussed [here](sw_comp_rtcc.md).

A write operation to an I2C slave register typically looks like this:

```
  //Write to slave
  i2c.beginTransmission(I2C_SLAVE_ADDR);
  i2c.write(I2C_SLAVE_REG_ADDR);
  i2c.write(I2C_SLAVE_REG_VAL);
  i2c.endTransmission();
```

A read operation goes like this:

```
    //Read from slave

    i2c.beginTransmission(I2C_SLAVE_ADDR);
    i2c.write(I2C_SLAVE_REG_ADDR);
    i2c.endTransmission();

    i2c.requestFrom(I2C_SLAVE_ADDR, 1 /*Request 1 byte*/);
    uint8_t i2cSlaveRegVal = i2c.read();
```

## Slave Register Protocol not assumed

The I2C API doesn't assume that the first byte of an I2C data frame is a slave register address. For the I2C  API, the first byte of a data frame is just that. No special role is assigned to it. This is a good thing because it works. Even though the WBI2C clearly *is* built around the slave register address convention, it can work with an I2C slave that doesn't follow this convention.

Imagine an I2C slave that accepts a stream of data bytes, without any particular format, and you want to send the following four bytes to it: 255, 254, 253, 252. Using the I2C API, you would write:

```
  uint8_t bytes[4] = {255, 254, 253, 252};

  i2c.beginTransmission(I2C_SLAVE_ADDR);
  for (int ii=0; ii<4; ii++) i2c.write(bytes[ii]);
  uint8_t res = i2c.endTransmission();
```

The I2C driver code will configure WBI2C as follows:

1. It writes the following values to the WBI2C Proxy memory:

    | WBI2C Proxy Memory Address | Value |
    |----------------------------|-------|
    | 255 | 254 |
    | 0   | 253 |
    | 1   | 252 |

2. It writes to the WBI2C CMD register, specifying a write transaction, setting the slave register address to 255 and the number of bytes to 3.

This tricks WBI2C into 'thinking' it's writing to slave register addresses 255, 0, and 1. The resulting I2C data frame consists of the byte sequence 255, 254, 253, 252, as intended.

This works because:

1. WBI2C wraps around to proxy memory address 0 if a multi-byte transaction continues past address 255.
2. WBI2C is configured with a proxy memory size of 256 bytes. The address space covers all byte values.

The only limitation is that a bulk transaction can't be more than 257 bytes long, corresponding to one (fake) slave register address byte plus 256 proxy memory bytes.

