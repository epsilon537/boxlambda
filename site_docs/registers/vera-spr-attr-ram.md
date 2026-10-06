# VERA Sprite Attribute RAM layout

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

Base address: 0x12001000

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [MODEADDR](#modeaddr)    | 0x00000000 | Sprite RAM Mode and VRAM Address Attribute (16-bit). |
| [X](#x)                  | 0x00000002 | Sprite RAM X Attribute (16-bit). |
| [Y](#y)                  | 0x00000004 | Sprite RAM Y Attribute (16-bit). |
| [FLAGS](#flags)          | 0x00000006 | Sprite RAM Flags Attribute (16-bit). |

## MODEADDR

Sprite RAM Mode and VRAM Address Attribute (16-bit).

Address offset: 0x00000000

Reset value: 0x0000

![modeaddr](md_img/modeaddr.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| MODE             | 15     | wo              | 0x0        | 0=4BPP mode, 1=8BPP mode. |
| -                | 14:12  | -               | 0x0        | Reserved |
| ADDR             | 11:0   | wo              | 0x000      | VRAM Address bits 16:5. |

Enumerated values for MODEADDR.MODE.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| 4BPP             | 0x0    | 4 bits per pixel sprite mode. |
| 8BPP             | 0x1    | 8 bits per pixel sprite mode. |

Back to [Register map](#register-map-summary).

## X

Sprite RAM X Attribute (16-bit).

Address offset: 0x00000002

Reset value: 0x0000

![x](md_img/x.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 15:10  | -               | 0x0        | Reserved |
| val              | 9:0    | wo              | 0x00       | Sprite X position. |

Back to [Register map](#register-map-summary).

## Y

Sprite RAM Y Attribute (16-bit).

Address offset: 0x00000004

Reset value: 0x0000

![y](md_img/y.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 15:10  | -               | 0x0        | Reserved |
| val              | 9:0    | wo              | 0x00       | Sprite Y position. |

Back to [Register map](#register-map-summary).

## FLAGS

Sprite RAM Flags Attribute (16-bit).

Address offset: 0x00000006

Reset value: 0x0000

![flags](md_img/flags.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| HEIGHT           | 15:14  | wo              | 0x0        | Sprite Height |
| WIDTH            | 13:12  | wo              | 0x0        | Sprite Width |
| PALOFFSET        | 11:8   | wo              | 0x0        | Sprite Palette Offset. |
| COLMASK          | 7:4    | wo              | 0x0        | Collision Mask |
| ZDEPTH           | 3:2    | wo              | 0x0        | Z Depth |
| FLIP             | 1:0    | wo              | 0x0        | Vertical and/or Horizontal Flip |

Enumerated values for FLAGS.FLIP.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| HFLIP            | 0x1    | Horizontal Flip |
| VFLIP            | 0x2    | Vertical Flip |
| HFLIP_VFLIP      | 0x3    | Horizonal and Vertical Flip |

Enumerated values for FLAGS.ZDEPTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| DIS              | 0x0    | Sprite disabled. |
| BG_L0            | 0x1    | Between background and L0. |
| L0_L1            | 0x2    | Between L0 and L1. |
| L1               | 0x3    | In front of L1. |

Enumerated values for FLAGS.WIDTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| W8               | 0x0    | 8 pixel sprite width |
| W16              | 0x1    | 16 pixel sprite width |
| W32              | 0x2    | 32 pixel sprite width |
| W64              | 0x3    | 64 pixel sprite width |

Enumerated values for FLAGS.HEIGHT.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| H8               | 0x0    | 8 pixel sprite height |
| H16              | 0x1    | 16 pixel sprite height |
| H32              | 0x2    | 32 pixel sprite height |
| H64              | 0x3    | 64 pixel sprite height |

Back to [Register map](#register-map-summary).
