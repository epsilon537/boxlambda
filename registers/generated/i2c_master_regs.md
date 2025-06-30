# I2C Master Register map

Created with [Corsair](https://github.com/esynr3z/corsair) v1.0.4.

## Conventions

| Access mode | Description               |
| :---------- | :------------------------ |
| rw          | Read and Write            |
| rw1c        | Read and Write 1 to Clear |
| rw1s        | Read and Write 1 to Set   |
| ro          | Read Only                 |
| roc         | Read Only to Clear        |
| roll        | Read Only / Latch Low     |
| rolh        | Read Only / Latch High    |
| wo          | Write only                |
| wosc        | Write Only / Self Clear   |

## Register map summary

Base address: 0x10000200

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [CMD](#cmd)              | 0x00000000 | I2C master command register |
| [SPD](#spd)              | 0x00000004 | Speed register |
| [ISR](#isr)              | 0x00000008 | Interrupt Status Register |
| [IEN](#ien)              | 0x0000000c | Interrupt Enable Register |

## CMD

I2C master command register

Address offset: 0x00000000

Reset value: 0x00000000

![cmd](md_img/cmd.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| BUSY             | 31     | ro              | 0x0        | Transaction ongoing. |
| ERR              | 30     | ro              | 0x0        | Command error |
| -                | 29:24  | -               | 0x0        | Reserved |
| SLAVE_ADDR       | 23:17  | rw              | 0x0        | I2C slave address |
| RD_N_WR          | 16     | rw              | 0x0        | Set to 1 for a read command, 0 for a write command. |
| START_ADDR       | 15:8   | rw              | 0x00       | Initial address to read from or write to. |
| NUM_BYTES        | 7:0    | rw              | 0x00       | Number of bytes to read/write. |

Back to [Register map](#register-map-summary).

## SPD

Speed register

Address offset: 0x00000004

Reset value: 0x00000000

![spd](md_img/spd.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:20  | -               | 0x000      | Reserved |
| VALUE            | 19:0   | rw              | 0x00000    | Number of system clocks for I2C wait state (1/4th of I2C bus clock period). |

Back to [Register map](#register-map-summary).

## ISR

Interrupt Status Register

Address offset: 0x00000008

Reset value: 0x00000000

![isr](md_img/isr.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:1   | -               | 0x0000000  | Reserved |
| BUSY             | 0      | rw1c            | 0x0        | Set when I2C goes from busy to idle. |

Back to [Register map](#register-map-summary).

## IEN

Interrupt Enable Register

Address offset: 0x0000000c

Reset value: 0x00000000

![ien](md_img/ien.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:1   | -               | 0x0000000  | Reserved |
| BUSY             | 0      | rw              | 0x0        | Set to enable BUSY interrupt. |

Back to [Register map](#register-map-summary).
