# VERA Register map

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

Base address: 0x12000000

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [CTRL_STATUS](#ctrl_status) | 0x00000000 | Control/Status register. |
| [DC_BORDER](#dc_border)  | 0x00000004 | Display composer border register. |
| [IEN](#ien)              | 0x00000008 | Interrupt enable register. |
| [ISR](#isr)              | 0x0000000c | Interrupt status register. |
| [IRQLINE](#irqline)      | 0x00000010 | Interrupt line register. |
| [SCANLINE](#scanline)    | 0x00000014 | Scanline register |
| [DC_VIDEO](#dc_video)    | 0x00000018 | Display composer video register. |
| [DC_HSCALE](#dc_hscale)  | 0x00000020 | Display composer horizontal scale register. |
| [DC_VSCALE](#dc_vscale)  | 0x00000024 | Display composer vertical scale register. |
| [DC_HSTART](#dc_hstart)  | 0x00000028 | Display composer horizontal start register. |
| [DC_HSTOP](#dc_hstop)    | 0x0000002c | Display compser horizontal stop register. |
| [DC_VSTART](#dc_vstart)  | 0x00000030 | Display composer vertical start register. |
| [DC_VSTOP](#dc_vstop)    | 0x00000034 | Display composer vertical stop register. |
| [L0_CONFIG](#l0_config)  | 0x00000040 | Layer 0 Configuration regiser. |
| [L0_MAPBASE](#l0_mapbase) | 0x00000044 | Layer 0 map base register. |
| [L0_TILEBASE](#l0_tilebase) | 0x00000048 | Layer 0 tile base register. |
| [L0_HSCROLL](#l0_hscroll) | 0x00000050 | Layer 0 horizontal scroll register. |
| [L0_VSCROLL](#l0_vscroll) | 0x00000054 | Layer 0 vertical scroll register. |
| [L1_CONFIG](#l1_config)  | 0x00000080 | Layer 1 Configuration regiser. |
| [L1_MAPBASE](#l1_mapbase) | 0x00000084 | Layer 1 map base register. |
| [L1_TILEBASE](#l1_tilebase) | 0x00000088 | Layer 1 tile base register. |
| [L1_HSCROLL](#l1_hscroll) | 0x00000090 | Layer 1 horizontal scroll register. |
| [L1_VSCROLL](#l1_vscroll) | 0x00000094 | Layer 1 vertical scroll register. |

## CTRL_STATUS

Control/Status register.

Address offset: 0x00000000

Reset value: 0x00000000

![ctrl_status](md_img/ctrl_status.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:2   | -               | 0x0000000  | Reserved |
| CAPTURE_EN       | 1      | rw              | 0x0        | Enable VGA line capture. Bit returns to 0 when capture has completed. |
| SBNK             | 0      | rw              | 0x0        | Active sprite bank. |

Back to [Register map](#register-map-summary).

## DC_BORDER

Display composer border register.

Address offset: 0x00000004

Reset value: 0x00000000

![dc_border](md_img/dc_border.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| BORDER_COLOR     | 7:0    | rw              | 0x00       | Border color |

Back to [Register map](#register-map-summary).

## IEN

Interrupt enable register.

Address offset: 0x00000008

Reset value: 0x00000000

![ien](md_img/ien.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:3   | -               | 0x0000000  | Reserved |
| SPRCOL           | 2      | rw              | 0x0        | Sprite collision interrupt enable. |
| LINE             | 1      | rw              | 0x0        | Line interrupt enable. |
| VSYNC            | 0      | rw              | 0x0        | Vertical sync interrupt enable. |

Back to [Register map](#register-map-summary).

## ISR

Interrupt status register.

Address offset: 0x0000000c

Reset value: 0x00000000

![isr](md_img/isr.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| SPR_COLLISIONS   | 7:4    | ro              | 0x0        | Sprite collisions as determined by sprite renderer. |
| -                | 3      | -               | 0x0        | Reserved |
| SPRCOL           | 2      | rw1c            | 0x0        | Sprite collision interrupt status. |
| LINE             | 1      | rw1c            | 0x0        | Line interrupt status. |
| VSYNC            | 0      | rw1c            | 0x0        | Vertical sync interrupt status. |

Back to [Register map](#register-map-summary).

## IRQLINE

Interrupt line register.

Address offset: 0x00000010

Reset value: 0x00000000

![irqline](md_img/irqline.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:10  | -               | 0x00000    | Reserved |
| VALUE            | 9:0    | rw              | 0x00       | Scanline on which to generate line interrupt. |

Back to [Register map](#register-map-summary).

## SCANLINE

Scanline register

Address offset: 0x00000014

Reset value: 0x00000000

![scanline](md_img/scanline.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:10  | -               | 0x00000    | Reserved |
| VALUE            | 9:0    | ro              | 0x00       | Current scanline. |

Back to [Register map](#register-map-summary).

## DC_VIDEO

Display composer video register.

Address offset: 0x00000018

Reset value: 0x00000000

![dc_video](md_img/dc_video.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:7   | -               | 0x000000   | Reserved |
| SPR_ENABLE       | 6      | rw              | 0x0        | Enable sprites. |
| L1_ENABLE        | 5      | rw              | 0x0        | Enable layer 1. |
| L0_ENABLE        | 4      | rw              | 0x0        | Enable Layer 0. |
| -                | 3:2    | -               | 0x0        | Reserved |
| OUTPUT_MODE      | 1:0    | rw              | 0x0        | Video output mode. |

Enumerated values for DC_VIDEO.OUTPUT_MODE.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| DIS              | 0x0    | Video disabled. |
| VGA              | 0x1    | VGA output enabled. |

Back to [Register map](#register-map-summary).

## DC_HSCALE

Display composer horizontal scale register.

Address offset: 0x00000020

Reset value: 0x00000000

![dc_hscale](md_img/dc_hscale.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | the horizonal fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel. |

Back to [Register map](#register-map-summary).

## DC_VSCALE

Display composer vertical scale register.

Address offset: 0x00000024

Reset value: 0x00000000

![dc_vscale](md_img/dc_vscale.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | the vertical fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel. |

Back to [Register map](#register-map-summary).

## DC_HSTART

Display composer horizontal start register.

Address offset: 0x00000028

Reset value: 0x00000000

![dc_hstart](md_img/dc_hstart.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:10  | -               | 0x00000    | Reserved |
| VALUE            | 9:0    | rw              | 0x00       | Horizontal start of active part of screen in 640x480 space. |

Back to [Register map](#register-map-summary).

## DC_HSTOP

Display compser horizontal stop register.

Address offset: 0x0000002c

Reset value: 0x00000000

![dc_hstop](md_img/dc_hstop.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:10  | -               | 0x00000    | Reserved |
| VALUE            | 9:0    | rw              | 0x00       | Horizontal stop of active part of screen in 640x480 space. |

Back to [Register map](#register-map-summary).

## DC_VSTART

Display composer vertical start register.

Address offset: 0x00000030

Reset value: 0x00000000

![dc_vstart](md_img/dc_vstart.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:10  | -               | 0x00000    | Reserved |
| VALUE            | 9:0    | rw              | 0x00       | Vertical start of active part of screen in 640x480 space. |

Back to [Register map](#register-map-summary).

## DC_VSTOP

Display composer vertical stop register.

Address offset: 0x00000034

Reset value: 0x00000000

![dc_vstop](md_img/dc_vstop.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:10  | -               | 0x00000    | Reserved |
| VALUE            | 9:0    | rw              | 0x00       | Vertical stop of active part of screen in 640x480 space. |

Back to [Register map](#register-map-summary).

## L0_CONFIG

Layer 0 Configuration regiser.

Address offset: 0x00000040

Reset value: 0x00000000

![l0_config](md_img/l0_config.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| MAP_HEIGHT       | 7:6    | rw              | 0x0        | Tile map height. |
| MAP_WIDTH        | 5:4    | rw              | 0x0        | Tile map width. |
| T256C            | 3      | rw              | 0x0        | When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes. |
| BITMAP_MODE      | 2      | rw              | 0x0        | 1 selects bitmap mode, 0 selects tile mode. |
| COLOR_DEPTH      | 1:0    | rw              | 0x0        | Number of bits pers pixel to encode color information. |

Enumerated values for L0_CONFIG.COLOR_DEPTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| ONE_BPP          | 0x0    | 1 bpp. |
| TWO_BPP          | 0x1    | 2 bpp. |
| FOUR_BPP         | 0x2    | 4 bpp. |
| EIGHT_BPP        | 0x3    | 8 bpp. |

Enumerated values for L0_CONFIG.MAP_WIDTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILES_32         | 0x0    | 32 tiles wide. |
| TILES_64         | 0x1    | 64 tiles wide. |
| TILES_128        | 0x2    | 128 tiles wide. |
| TILES_256        | 0x3    | 256 tiles wide. |

Enumerated values for L0_CONFIG.MAP_HEIGHT.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILES_32         | 0x0    | 32 tiles high. |
| TILES_64         | 0x1    | 64 tiles high. |
| TILES_128        | 0x2    | 128 tiles high. |
| TILES_256        | 0x3    | 256 tiles high. |

Back to [Register map](#register-map-summary).

## L0_MAPBASE

Layer 0 map base register.

Address offset: 0x00000044

Reset value: 0x00000000

![l0_mapbase](md_img/l0_mapbase.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| ADDR_16_9        | 7:0    | rw              | 0x00       | Bits 16:9 of the base address of the tile map. |

Back to [Register map](#register-map-summary).

## L0_TILEBASE

Layer 0 tile base register.

Address offset: 0x00000048

Reset value: 0x00000000

![l0_tilebase](md_img/l0_tilebase.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| TILE_BASE_ADDR_16_11 | 7:2    | rw              | 0x0        | Bits 16:11 of the base address of the tile data. |
| TILE_HEIGHT      | 1      | rw              | 0x0        | Tile height. |
| TILE_BITMAP_WIDTH | 0      | rw              | 0x0        | Tile or Bitmap width. |

Enumerated values for L0_TILEBASE.TILE_BITMAP_WIDTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILE_BITMAP_W_8_320 | 0x0    | 8 pixel tile width, 320 pixels bitmap width. |
| TILE_BITMAP_W_16_640 | 0x1    | 16 pixel tile width, 640 pixels bitmap width. |

Enumerated values for L0_TILEBASE.TILE_HEIGHT.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILE_HEIGHT_8    | 0x0    | 8 pixel tile height. |
| TILE_HEIGHT_16   | 0x1    | 16 pixel tile height. |

Back to [Register map](#register-map-summary).

## L0_HSCROLL

Layer 0 horizontal scroll register.

Address offset: 0x00000050

Reset value: 0x00000000

![l0_hscroll](md_img/l0_hscroll.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:12  | -               | 0x00000    | Reserved |
| HSCROLL_11_8_PAL_OFFSET | 11:8   | rw              | 0x0        | In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors. |
| HSCROLL_7_0      | 7:0    | rw              | 0x00       | Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right. |

Back to [Register map](#register-map-summary).

## L0_VSCROLL

Layer 0 vertical scroll register.

Address offset: 0x00000054

Reset value: 0x00000000

![l0_vscroll](md_img/l0_vscroll.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:12  | -               | 0x00000    | Reserved |
| VALUE            | 11:0   | rw              | 0x000      | Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down. |

Back to [Register map](#register-map-summary).

## L1_CONFIG

Layer 1 Configuration regiser.

Address offset: 0x00000080

Reset value: 0x00000000

![l1_config](md_img/l1_config.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| MAP_HEIGHT       | 7:6    | rw              | 0x0        | Tile map height. |
| MAP_WIDTH        | 5:4    | rw              | 0x0        | Tile map width. |
| T256C            | 3      | rw              | 0x0        | When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes. |
| BITMAP_MODE      | 2      | rw              | 0x0        | 1 selects bitmap mode, 0 selects tile mode. |
| COLOR_DEPTH      | 1:0    | rw              | 0x0        | Number of bits pers pixel to encode color information. |

Enumerated values for L1_CONFIG.COLOR_DEPTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| ONE_BPP          | 0x0    | 1 bpp. |
| TWO_BPP          | 0x1    | 2 bpp. |
| FOUR_BPP         | 0x2    | 4 bpp. |
| EIGHT_BPP        | 0x3    | 8 bpp. |

Enumerated values for L1_CONFIG.MAP_WIDTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILES_32         | 0x0    | 32 tiles wide. |
| TILES_64         | 0x1    | 64 tiles wide. |
| TILES_128        | 0x2    | 128 tiles wide. |
| TILES_256        | 0x3    | 256 tiles wide. |

Enumerated values for L1_CONFIG.MAP_HEIGHT.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILES_32         | 0x0    | 32 tiles high. |
| TILES_64         | 0x1    | 64 tiles high. |
| TILES_128        | 0x2    | 128 tiles high. |
| TILES_256        | 0x3    | 256 tiles high. |

Back to [Register map](#register-map-summary).

## L1_MAPBASE

Layer 1 map base register.

Address offset: 0x00000084

Reset value: 0x00000000

![l1_mapbase](md_img/l1_mapbase.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| ADDR_16_9        | 7:0    | rw              | 0x00       | Bits 16:9 of the base address of the tile map. |

Back to [Register map](#register-map-summary).

## L1_TILEBASE

Layer 1 tile base register.

Address offset: 0x00000088

Reset value: 0x00000000

![l1_tilebase](md_img/l1_tilebase.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| TILE_BASE_ADDR_16_11 | 7:2    | rw              | 0x0        | Bits 16:11 of the base address of the tile data. |
| TILE_HEIGHT      | 1      | rw              | 0x0        | Tile height. |
| TILE_BITMAP_WIDTH | 0      | rw              | 0x0        | Tile or Bitmap width. |

Enumerated values for L1_TILEBASE.TILE_BITMAP_WIDTH.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILE_BITMAP_W_8_320 | 0x0    | 8 pixel tile width, 320 pixels bitmap width. |
| TILE_BITMAP_W_16_640 | 0x1    | 16 pixel tile width, 640 pixels bitmap width. |

Enumerated values for L1_TILEBASE.TILE_HEIGHT.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| TILE_HEIGHT_8    | 0x0    | 8 pixel tile height. |
| TILE_HEIGHT_16   | 0x1    | 16 pixel tile height. |

Back to [Register map](#register-map-summary).

## L1_HSCROLL

Layer 1 horizontal scroll register.

Address offset: 0x00000090

Reset value: 0x00000000

![l1_hscroll](md_img/l1_hscroll.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:12  | -               | 0x00000    | Reserved |
| HSCROLL_11_8_PAL_OFFSET | 11:8   | rw              | 0x0        | In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors. |
| HSCROLL_7_0      | 7:0    | rw              | 0x00       | Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right. |

Back to [Register map](#register-map-summary).

## L1_VSCROLL

Layer 1 vertical scroll register.

Address offset: 0x00000094

Reset value: 0x00000000

![l1_vscroll](md_img/l1_vscroll.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:12  | -               | 0x00000    | Reserved |
| VALUE            | 11:0   | rw              | 0x000      | Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down. |

Back to [Register map](#register-map-summary).
