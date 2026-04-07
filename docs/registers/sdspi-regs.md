# SDSPI Register map

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

Base address: 0x10000020

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [CMD](#cmd)              | 0x00000000 | Command and status register |
| [DAT](#dat)              | 0x00000004 | Return data/argument register |
| [FIFO_0](#fifo_0)        | 0x00000008 | 128 word FIFO[0] data |
| [FIFO_1](#fifo_1)        | 0x0000000c | 128 word FIFO[1] data |
| [ISR](#isr)              | 0x00000010 | Interrupt status register |
| [IEN](#ien)              | 0x00000014 | Interrupt enable register |

## CMD

Command and status register

Address offset: 0x00000000

Reset value: 0x00000000

![cmd](md_img/cmd.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:20  | -               | 0x000      | Reserved |
| P                | 19     | ro              | 0x0        | 1 = card missing, 0 = card present. |
| REM              | 18     | rw1c            | 0x0        | Card has been removed since last read. If P=0 and R=1, card has been inserted and needs initialization. |
| -                | 17:16  | -               | 0x0        | Reserved |
| ERR              | 15     | rw1c            | 0x0        | Error indication. Time-out, card reset, CRC error, R1 response error. |
| BUSY             | 14     | ro              | 0x0        | Busy bit, set while command is running. |
| -                | 13     | -               | 0x0        | Reserved |
| SEL              | 12     | rw              | 0x0        | 1 = select FIFO[0], 0 = select FIFO[1]. |
| F                | 11     | rw              | 0x0        | Set if FIFO data transmision accompanies command. |
| WR               | 10     | rw              | 0x0        | 1 to write from FIFO to card, 0 to read from card into FIFO. Assumes F is set. |
| ERESP            | 9:8    | rw              | 0x0        | Expected response. |
| R1_CMD           | 7:0    | rw              | 0x00       | On write, if bits [7:6]==01 and card idle, remaining bits are sent to card. Contains R1 response when command has completed. |

Enumerated values for CMD.ERESP.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| R1_RESP          | 0x0    | Expect R1 response. |
| R1B_RESP         | 0x1    | Expect R1b response. |
| R2_R3_R7         | 0x2    | Expect R2, R3, R7 32-bit response. |

Back to [Register map](#register-map-summary).

## DAT

Return data/argument register

Address offset: 0x00000004

Reset value: 0x00000000

![dat](md_img/dat.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Command argument, command response. R2 response is in upper 8-bits. |

Back to [Register map](#register-map-summary).

## FIFO_0

128 word FIFO[0] data

Address offset: 0x00000008

Reset value: 0x00000000

![fifo_0](md_img/fifo_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Data read from or to write to card. |

Back to [Register map](#register-map-summary).

## FIFO_1

128 word FIFO[1] data

Address offset: 0x0000000c

Reset value: 0x00000000

![fifo_1](md_img/fifo_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Data read from or to write to card. |

Back to [Register map](#register-map-summary).

## ISR

Interrupt status register

Address offset: 0x00000010

Reset value: 0x00000000

![isr](md_img/isr.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:2   | -               | 0x0000000  | Reserved |
| CARD_REMOVED     | 1      | rw1c            | 0x0        | Set when controller detects that the SD card has been removed. |
| BUSY             | 0      | rw1c            | 0x0        | Set when controller goes from busy to non-busy state |

Back to [Register map](#register-map-summary).

## IEN

Interrupt enable register

Address offset: 0x00000014

Reset value: 0x00000000

![ien](md_img/ien.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:2   | -               | 0x0000000  | Reserved |
| CARD_REMOVED     | 1      | rw              | 0x0        | Set to enabled CARD_REMOVED interrupt. |
| BUSY             | 0      | rw              | 0x0        | Set to enable BUSY interrupt. |

Back to [Register map](#register-map-summary).
