# USB HID Register map

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

Base address: 0x10000080

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [IEN](#ien)              | 0x00000000 | Interrupt Enable Register |
| [ISR](#isr)              | 0x00000004 | Interrupt Status Register |
| [STATUS](#status)        | 0x00000008 | Status Register |
| [KEY_MODS](#key_mods)    | 0x0000000c | Key modifiers |
| [KEYS](#keys)            | 0x00000010 | Keys register |
| [MOUSE](#mouse)          | 0x00000014 | Mouse register |
| [GAME](#game)            | 0x00000018 | Game pad register |
| [REPORT_0](#report_0)    | 0x0000001c | USB report 0 register |
| [REPORT_1](#report_1)    | 0x00000020 | USB report 1 register |
| [LEDS](#leds)            | 0x00000024 | LED register |

## IEN

Interrupt Enable Register

Address offset: 0x00000000

Reset value: 0x00000000

![ien](md_img/ien.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:2   | -               | 0x0000000  | Reserved |
| LED_REPORT       | 1      | rw              | 0x0        | Set to enable LED_REPORT interrupt. |
| USB_REPORT       | 0      | rw              | 0x0        | Set to enable USB_REPORT interrupt. |

Back to [Register map](#register-map-summary).

## ISR

Interrupt Status Register

Address offset: 0x00000004

Reset value: 0x00000000

![isr](md_img/isr.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:2   | -               | 0x0000000  | Reserved |
| LED_REPORT       | 1      | rw1c            | 0x0        | Set when a LED report is received. |
| USB_REPORT       | 0      | rw1c            | 0x0        | Set when a USB report is received. |

Back to [Register map](#register-map-summary).

## STATUS

Status Register

Address offset: 0x00000008

Reset value: 0x00000000

![status](md_img/status.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:3   | -               | 0x0000000  | Reserved |
| CONN_ERR         | 2      | ro              | 0x0        | Connection error. |
| USB_TYP          | 1:0    | ro              | 0x0        | USB type |

Enumerated values for STATUS.USB_TYP.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| NO_DEV           | 0x0    | No device |
| KEYB             | 0x1    | Keyboard |
| MOUSE            | 0x2    | Mouse |
| GAME             | 0x3    | Game pad |

Back to [Register map](#register-map-summary).

## KEY_MODS

Key modifiers

Address offset: 0x0000000c

Reset value: 0x00000000

![key_mods](md_img/key_mods.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| RMETA            | 7      | ro              | 0x0        | Right meta |
| RALT             | 6      | ro              | 0x0        | Right alt |
| RSHIFT           | 5      | ro              | 0x0        | Right shift |
| RCTRL            | 4      | ro              | 0x0        | Right control |
| LMETA            | 3      | ro              | 0x0        | Left meta |
| LALT             | 2      | ro              | 0x0        | Left alt |
| LSHIFT           | 1      | ro              | 0x0        | Left shift |
| LCTRL            | 0      | ro              | 0x0        | Left control |

Back to [Register map](#register-map-summary).

## KEYS

Keys register

Address offset: 0x00000010

Reset value: 0x00000000

![keys](md_img/keys.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| KEY_3            | 31:24  | ro              | 0x00       | Key 3 |
| KEY_2            | 23:16  | ro              | 0x00       | Key 2 |
| KEY_1            | 15:8   | ro              | 0x00       | Key 1 |
| KEY_0            | 7:0    | ro              | 0x00       | Key 0 |

Back to [Register map](#register-map-summary).

## MOUSE

Mouse register

Address offset: 0x00000014

Reset value: 0x00000000

![mouse](md_img/mouse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:19  | -               | 0x000      | Reserved |
| BTN_MIDDLE       | 18     | ro              | 0x0        | Middle button |
| BTN_RIGHT        | 17     | ro              | 0x0        | Right button |
| BTN_LEFT         | 16     | ro              | 0x0        | Left button |
| DX               | 15:8   | ro              | 0x00       | Delta x |
| DY               | 7:0    | ro              | 0x00       | Delta y |

Back to [Register map](#register-map-summary).

## GAME

Game pad register

Address offset: 0x00000018

Reset value: 0x00000000

![game](md_img/game.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | ro              | 0x00000000 | TBD |

Back to [Register map](#register-map-summary).

## REPORT_0

USB report 0 register

Address offset: 0x0000001c

Reset value: 0x00000000

![report_0](md_img/report_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | ro              | 0x00000000 | USB report 0 value |

Back to [Register map](#register-map-summary).

## REPORT_1

USB report 1 register

Address offset: 0x00000020

Reset value: 0x00000000

![report_1](md_img/report_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | ro              | 0x00000000 | USB report 1 value |

Back to [Register map](#register-map-summary).

## LEDS

LED register

Address offset: 0x00000024

Reset value: 0x00000000

![leds](md_img/leds.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| COMPOSE          | 3      | rw              | 0x0        | Compose LED |
| SCROLL_LOCK      | 2      | rw              | 0x0        | SCROLL lock LED |
| CAPS_LOCK        | 1      | rw              | 0x0        | CAPS lock LED |
| NUM_LOCK         | 0      | rw              | 0x0        | Num lock LED |

Back to [Register map](#register-map-summary).
