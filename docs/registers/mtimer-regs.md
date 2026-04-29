# Machine Timer Register map

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

Base address: 0x10020000

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [MTIME](#mtime)          | 0x00000000 | Machine-level time counter, low word. |
| [MTIMEH](#mtimeh)        | 0x00000004 | Machine-level time counter, high word. |
| [MTIMECMP](#mtimecmp)    | 0x00000008 | Machine-level time compare, low word. |
| [MTIMECMPH](#mtimecmph)  | 0x0000000c | Machine-level time compare, high word. |
| [MTIMEBLK](#mtimeblk)    | 0x00000010 | Blocking time compare register. |

## MTIME

Machine-level time counter, low word.

Address offset: 0x00000000

Reset value: 0x00000000

![mtime](md_img/mtime.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Machine-level time counter, low word. |

Back to [Register map](#register-map-summary).

## MTIMEH

Machine-level time counter, high word.

Address offset: 0x00000004

Reset value: 0x00000000

![mtimeh](md_img/mtimeh.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Machine-level time counter, high word. |

Back to [Register map](#register-map-summary).

## MTIMECMP

Machine-level time compare, low word.

Address offset: 0x00000008

Reset value: 0x00000000

![mtimecmp](md_img/mtimecmp.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Machine-level time compare, low word. |

Back to [Register map](#register-map-summary).

## MTIMECMPH

Machine-level time compare, high word.

Address offset: 0x0000000c

Reset value: 0x00000000

![mtimecmph](md_img/mtimecmph.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Machine-level time compare, high word. |

Back to [Register map](#register-map-summary).

## MTIMEBLK

Blocking time compare register.

Address offset: 0x00000010

Reset value: 0x00000000

![mtimeblk](md_img/mtimeblk.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | A write operation to this register blocks the CPU until the lower 8 bits of the MTIME register match the written value. |

Back to [Register map](#register-map-summary).
