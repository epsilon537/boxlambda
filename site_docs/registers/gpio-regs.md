# GPIO Register map

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

Base address: 0x10000100

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [RGPIO_IN](#rgpio_in)    | 0x00000000 | Latched value of general-purpose input pins. |
| [RGPIO_OUT](#rgpio_out)  | 0x00000004 | General-purpose output pin values. |
| [RGPIO_OE](#rgpio_oe)    | 0x00000008 | General-purpose pins output enables. |
| [RGPIO_INTE](#rgpio_inte) | 0x0000000c | General-purpose pin interrupt enables. |
| [RGPIO_PTRIG](#rgpio_ptrig) | 0x00000010 | Trigger IRQ on positive edge. |
| [RGPIO_CTRL_STATUS](#rgpio_ctrl_status) | 0x00000018 | GPIO control and status register |
| [RGPIO_INTS](#rgpio_ints) | 0x0000001c | GPIO interrupt status register. |
| [RGPIO_ECLK](#rgpio_eclk) | 0x00000020 | Latch on gp_clk input signal. |
| [RGPIO_NEC](#rgpio_nec)  | 0x00000024 | Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set. |

## RGPIO_IN

Latched value of general-purpose input pins.

Address offset: 0x00000000

Reset value: 0x00000000

![rgpio_in](md_img/rgpio_in.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | ro              | 0x00000000 | Latched value of general-purpose input pins. |

Back to [Register map](#register-map-summary).

## RGPIO_OUT

General-purpose output pin values.

Address offset: 0x00000004

Reset value: 0x00000000

![rgpio_out](md_img/rgpio_out.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | General-purpose output pin values. |

Back to [Register map](#register-map-summary).

## RGPIO_OE

General-purpose pins output enables.

Address offset: 0x00000008

Reset value: 0x00000000

![rgpio_oe](md_img/rgpio_oe.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | General-purpose pins output enables. |

Back to [Register map](#register-map-summary).

## RGPIO_INTE

General-purpose pin interrupt enables.

Address offset: 0x0000000c

Reset value: 0x00000000

![rgpio_inte](md_img/rgpio_inte.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | General-purpose pins interrupt enables. |

Back to [Register map](#register-map-summary).

## RGPIO_PTRIG

Trigger IRQ on positive edge.

Address offset: 0x00000010

Reset value: 0x00000000

![rgpio_ptrig](md_img/rgpio_ptrig.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | Trigger IRQ on positive edge if set, on negative edge if cleared. |

Back to [Register map](#register-map-summary).

## RGPIO_CTRL_STATUS

GPIO control and status register

Address offset: 0x00000018

Reset value: 0x00000000

![rgpio_ctrl_status](md_img/rgpio_ctrl_status.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:2   | -               | 0x0000000  | Reserved |
| INTS             | 1      | ro              | 0x0        | Interrupt status |
| INTE             | 0      | rw              | 0x0        | Interrupt enabled |

Back to [Register map](#register-map-summary).

## RGPIO_INTS

GPIO interrupt status register.

Address offset: 0x0000001c

Reset value: 0x00000000

![rgpio_ints](md_img/rgpio_ints.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | Interrupt status - Write 0 to clear. |

Back to [Register map](#register-map-summary).

## RGPIO_ECLK

Latch on gp_clk input signal.

Address offset: 0x00000020

Reset value: 0x00000000

![rgpio_eclk](md_img/rgpio_eclk.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | When set, the gp_clk input signal is used to latch pin. |

Back to [Register map](#register-map-summary).

## RGPIO_NEC

Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set.

Address offset: 0x00000024

Reset value: 0x00000000

![rgpio_nec](md_img/rgpio_nec.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| PINS             | 31:0   | rw              | 0x00000000 | When set, gp_clk is active on negative edge, when cleared on positive edge. |

Back to [Register map](#register-map-summary).
