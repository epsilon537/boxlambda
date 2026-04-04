# SPIFlash Register map

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

Base address: 0x100000c0

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [CTRL](#ctrl)            | 0x00000000 | SPIFlash Control Register |

## CTRL

SPIFlash Control Register

Address offset: 0x00000000

Reset value: 0x00000000

![ctrl](md_img/ctrl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:9   | -               | 0x00000    | Reserved |
| CS_N             | 8      | rw              | 0x0        | 1/0 de/activates the control port. |
| DATA             | 7:0    | rw              | 0x00       | If control port is active, written byte value is sent out to SPI slave, top bit first. After write, may be read to retrieve return data byte. |

Back to [Register map](#register-map-summary).
