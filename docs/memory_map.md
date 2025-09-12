# BoxLambda Memory Map

Next to memory-mapped registers, BoxLambda includes the following memory
regions:

## IMEM

Low- and fixed-latency read-write internal memory.

- **Address Range**: `0x00000000-0x00040000`
- **Size**: 256 Kbytes

## I2C Master RAM

Read-write memory buffer for I2C transfers.

- **Address Range**: `0x10000300-0x10000400`
- **Size**: 256 bytes

## SPI Flash

High-latency read-only memory (writable through flash driver API).

- **Address Range**: `0x11000000-0x12000000`
- **Size**: 16 Mbytes

## VERA Sprite Attribute RAM

Write-only Sprite attribute RAM.

- **Address Range**: `0x12001000-0x12001400`
- **Size**: 1 Kbyte

Two banks of 64 entries of the following format:

<table>
	<tr>
		<th>Word</th>
		<th>31/15</th>
		<th>30/14</th>
		<th>29/13</th>
		<th>28/12</th>
		<th>27/11</th>
		<th>26/10</th>
		<th>25/9</th>
		<th>24/8</th>
		<th>23/7</th>
		<th>22/6</th>
		<th>21/5</th>
		<th>20/4</th>
		<th>19/3</th>
		<th>18/2</th>
		<th>17/1</th>
		<th>16/0</th>
	</tr>
		<tr>
		<td>Word 0 Bits 15-0</td>
		<td align="center" colspan="1">Mode</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="12">Address (16:5)</td>
	</tr>
	<tr>
		<td>Word 0 Bits 31-16</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="10">X</td>
	</tr>
	<tr>
		<td>Word 1 Bits 15-0</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="1">-</td>
		<td align="center" colspan="10">Y</td>
	</tr>
	<tr>
		<td>Word 1 Bits 31-16</td>
		<td align="center" colspan="2">Sprite height</td>
		<td align="center" colspan="2">Sprite width</td>
		<td align="center" colspan="4">Palette offset</td>
		<td align="center" colspan="4">Collision mask</td>
		<td align="center" colspan="2">Z-depth</td>
		<td align="center" colspan="1">V-flip</td>
		<td align="center" colspan="1">H-flip</td>
	</tr>
</table>

| Mode | Description |
| ---- | ----------- |
| 0    | 4 bpp       |
| 1    | 8 bpp       |

| Z-depth | Description                           |
| ------- | ------------------------------------- |
| 0       | Sprite disabled                       |
| 1       | Sprite between background and layer 0 |
| 2       | Sprite between layer 0 and layer 1    |
| 3       | Sprite in front of layer 1            |

| Sprite width / height | Description |
| --------------------- | ----------- |
| 0                     | 8 pixels    |
| 1                     | 16 pixels   |
| 2                     | 32 pixels   |
| 3                     | 64 pixels   |

**Rendering Priority** The sprite memory location dictates the order in which it is rendered. The sprite whose attributes are at the lowest location will be rendered in front of all other sprites; the sprite at the highest location will be rendered behind all other sprites, and so forth.

**Palette offset** works in the same way as with the layers.

The active sprite bank is selected by the SBNK bit in the CTRL register.

The Sprite Renderer can render a maximum of 512 sprite pixels per scanline. That corresponds to 64 8-pixel-wide sprites or 8 64-pixel-wide sprites. The 512 sprite-pixel-per-scanline limit is guaranteed regardless of Layer 0/1 configuration or Wishbone bus access load on Video RAM.

## VERA Palette RAM

Write-only color palette RAM.

- **Address Range**: `0x12002000-0x12002200`
- **Size**: 512 bytes

The palette translates 8-bit color indexes into 12-bit output colors. The palette has 256 entries, each with the following format:

<table>
	<tr>
		<th>Bit&nbsp;31-12</th>
		<th>Bit&nbsp;11-8</th>
		<th>Bit&nbsp;7-4</th>
		<th>Bit&nbsp;3-0</th>
	</tr>
	<tr>
		<td align="center">-</td>
		<td align="center">Red</td>
		<td align="center">Green</td>
		<td align="center">Blue</td>
	</tr>
</table>

At reset, the palette RAM will contain the following predefined palette:

    000,fff,800,afe,c4c,0c5,00a,ee7,d85,640,f77,333,777,af6,08f,bbb
    000,111,222,333,444,555,666,777,888,999,aaa,bbb,ccc,ddd,eee,fff
    211,433,644,866,a88,c99,fbb,211,422,633,844,a55,c66,f77,200,411
    611,822,a22,c33,f33,200,400,600,800,a00,c00,f00,221,443,664,886
    aa8,cc9,feb,211,432,653,874,a95,cb6,fd7,210,431,651,862,a82,ca3
    fc3,210,430,640,860,a80,c90,fb0,121,343,564,786,9a8,bc9,dfb,121
    342,463,684,8a5,9c6,bf7,120,241,461,582,6a2,8c3,9f3,120,240,360
    480,5a0,6c0,7f0,121,343,465,686,8a8,9ca,bfc,121,242,364,485,5a6
    6c8,7f9,020,141,162,283,2a4,3c5,3f6,020,041,061,082,0a2,0c3,0f3
    122,344,466,688,8aa,9cc,bff,122,244,366,488,5aa,6cc,7ff,022,144
    166,288,2aa,3cc,3ff,022,044,066,088,0aa,0cc,0ff,112,334,456,668
    88a,9ac,bcf,112,224,346,458,56a,68c,79f,002,114,126,238,24a,35c
    36f,002,014,016,028,02a,03c,03f,112,334,546,768,98a,b9c,dbf,112
    324,436,648,85a,96c,b7f,102,214,416,528,62a,83c,93f,102,204,306
    408,50a,60c,70f,212,434,646,868,a8a,c9c,fbe,211,423,635,847,a59
    c6b,f7d,201,413,615,826,a28,c3a,f3c,201,403,604,806,a08,c09,f0b

- Color indexes 0-15 contain the C64 color palette.
- Color indexes 16-31 contain a grayscale ramp.
- Color indexes 32-255 contain various hues, saturation levels, brightness levels.

## VERA VGA Line Capture RAM

Read-only RAM containing one VGA scanline worth of captured pixel data.

- **Address Range**: `0x12003000-0x12003a00`
- **Size**: 2560 bytes

The memory has 640 entries, one entry per displayed pixel in the captured
scanline (VERA always outputs 640x480 VGA). Each entry has the following format:

<table>
	<tr>
		<th>Bit&nbsp;31-12</th>
		<th>Bit&nbsp;11-8</th>
		<th>Bit&nbsp;7-4</th>
		<th>Bit&nbsp;3-0</th>
	</tr>
	<tr>
		<td align="center">-</td>
		<td align="center">Red</td>
		<td align="center">Green</td>
		<td align="center">Blue</td>
	</tr>
</table>

See [here](components_vera.md#the-video-vga-block-vga-line-capture) for a description of the VGA line capture mechanism.

## VERA VRAM

Low- and fixed-latency video memory.

- **Address Range**: `0x12040000-0x12060000`
- **Size**: 128 Kbytes

## SDRAM

High- and variable latency read-write external memory.

- **Address Range**: `0x20000000-0x30000000`
- **Size**: 256 Mbytes

