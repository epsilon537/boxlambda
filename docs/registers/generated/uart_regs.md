# UART Register map

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

Base address: 0x10010000

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [SETUP](#setup)          | 0x00000000 | Setup register |
| [FIFO](#fifo)            | 0x00000004 | Rx and Tx FIFO size and status |
| [RXDATA](#rxdata)        | 0x00000008 | Rx data register. |
| [TXDATA](#txdata)        | 0x0000000c | Tx data register. |
| [ISR](#isr)              | 0x00000010 | Interrupt status register |
| [IEN](#ien)              | 0x00000014 | Interrupt enable register |

## SETUP

Setup register

Address offset: 0x00000000

Reset value: 0x000001b2

![setup](md_img/setup.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31     | -               | 0x0        | Reserved |
| H                | 30     | rw              | 0x0        | Disable hardware flow control |
| N                | 29:28  | rw              | 0x0        | 8 - number of bits per word |
| S                | 27     | rw              | 0x0        | Number of stop bits - 1 |
| PFT              | 26:24  | rw              | 0x0        | Parity setup |
| BAUD_CLKS        | 23:0   | rw              | 0x0001b2   | System clock per baudrate interval |

Enumerated values for SETUP.BAUD_CLKS.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| B115200          | 0x0001b2 | 115200 baud |
| B9600            | 0x001458 | 9600 baud |

Enumerated values for SETUP.PFT.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| P_NONE           | 0x0    | No parity |
| P_ODD            | 0x4    | Odd Parity |
| P_EVEN           | 0x5    | Even Parity |
| P_SPC            | 0x6    | Party bit is a space |
| P_MK             | 0x7    | Party bit is a mark |

Enumerated values for SETUP.S.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| STOP_1           | 0x0    | One stop bit |
| STOP_2           | 0x1    | Two stop bits |

Enumerated values for SETUP.N.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| BPW_8            | 0x0    | 8 bits per word |
| BPW_7            | 0x1    | 7 bits per word |
| BPW_6            | 0x2    | 6 bits per word |
| BPW_5            | 0x3    | 5 bits per word |

Enumerated values for SETUP.H.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| HFL_EN           | 0x0    | Hardware flow control enabled |
| HFL_DIS          | 0x1    | Hardware flow control disabled |

Back to [Register map](#register-map-summary).

## FIFO

Rx and Tx FIFO size and status

Address offset: 0x00000004

Reset value: 0x00000000

![fifo](md_img/fifo.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| TX_LGLN          | 31:28  | ro              | 0x0        | Log base 2 of FIFO length. |
| TX_FILL          | 27:18  | ro              | 0x00       | Number of available spaces in Tx FIFO. |
| TX_H             | 17     | ro              | 0x0        | Tx FIFO high order fill bit set. |
| TX_Z             | 16     | ro              | 0x0        | Space available in Tx FIFO. |
| RX_LGLN          | 15:12  | ro              | 0x0        | Log base 2 of FIFO length. |
| RX_FILL          | 11:2   | ro              | 0x00       | Number of filled entries in Rx FIFO. |
| RX_H             | 1      | ro              | 0x0        | Rx FIFO high order fill bit set. |
| RX_Z             | 0      | ro              | 0x0        | Data available in Rx FIFO. |

Back to [Register map](#register-map-summary).

## RXDATA

Rx data register.

Address offset: 0x00000008

Reset value: 0x00000000

![rxdata](md_img/rxdata.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:13  | -               | 0x0000     | Reserved |
| E                | 12     | rw1c            | 0x0        | Read indicates Rx FIFO has overflowed since last reset. Writing 1 clears FIFO and waits for line idle before receiving next byte. |
| B                | 11     | rw1c            | 0x0        | Rx line is in break condition. |
| F                | 10     | rw1c            | 0x0        | Frame error. |
| P                | 9      | rw1c            | 0x0        | Parity error. |
| S                | 8      | ro              | 0x0        | Data invalid. |
| RWORD            | 7:0    | ro              | 0x00       | Read data word. |

Enumerated values for RXDATA.S.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| RWORD_VALID      | 0x0    | RWORD is valid. |
| RWORD_INVALID    | 0x1    | RWORD is invalid. |

Back to [Register map](#register-map-summary).

## TXDATA

Tx data register.

Address offset: 0x0000000c

Reset value: 0x00000000

![txdata](md_img/txdata.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:16  | -               | 0x0000     | Reserved |
| R                | 15     | ro              | 0x0        | Received RTS instantaneous value. |
| H                | 14     | ro              | 0x0        | Tx FIFO at least half full. |
| Z                | 13     | ro              | 0x0        | Tx FIFO not full. |
| E                | 12     | rw1c            | 0x0        | Read indicates Tx FIFO has overflowed since last reset. Writing 1 resets FIFO. |
| -                | 11:10  | -               | 0x0        | Reserved |
| B                | 9      | rw              | 0x0        | Tx break condition. |
| S                | 8      | ro              | 0x0        | Transmit busy. |
| TWORD            | 7:0    | rw              | 0x00       | Transmit data word. |

Back to [Register map](#register-map-summary).

## ISR

Interrupt status register

Address offset: 0x00000010

Reset value: 0x00000000

![isr](md_img/isr.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| TX_FIFO_HALF_EMPTY | 3      | rw1c            | 0x0        | Tx FIFO filling level dropped below the half empty threshold. |
| TX_FIFO_EMPTY    | 2      | rw1c            | 0x0        | Tx FIFO went from non-empty to empty state. |
| RX_FIFO_HALF_FULL | 1      | rw1c            | 0x0        | Receive FIFO passed the half-full threshold. |
| RX_DATA_AVL      | 0      | rw1c            | 0x0        | Rx FIFO went from empty to non-empty state. |

Back to [Register map](#register-map-summary).

## IEN

Interrupt enable register

Address offset: 0x00000014

Reset value: 0x00000000

![ien](md_img/ien.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| TX_FIFO_HALF_EMPTY | 3      | rw              | 0x0        | Enabled TX_FIFO_HALF_EMPTY interrupt. |
| TX_FIFO_EMPTY    | 2      | rw              | 0x0        | Enable TX_FIFO_EMPTY interrupt. |
| RX_FIFO_HALF_FULL | 1      | rw              | 0x0        | Enable RX_FIFO_HALF_FULL interrupt. |
| RX_DATA_AVL      | 0      | rw              | 0x0        | Enable RX_DATA_AVL interrupt. |

Back to [Register map](#register-map-summary).
