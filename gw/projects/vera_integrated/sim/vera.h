#ifndef VERA_H_
#define VERA_H

#define VERA_BASE 0

#define VERA_ADDR_L   	  (VERA_BASE + 0x00<<2)
#define VERA_ADDR_M   	  (VERA_BASE + 0x01<<2)
#define VERA_ADDR_H   	  (VERA_BASE + 0x02<<2)
#define VERA_DATA0        (VERA_BASE + 0x03<<2)
#define VERA_DATA1        (VERA_BASE + 0x04<<2)
#define VERA_CTRL         (VERA_BASE + 0x05<<2)

#define VERA_IEN          (VERA_BASE + 0x06<<2)
#define VERA_ISR          (VERA_BASE + 0x07<<2)
#define VERA_IRQ_LINE_L   (VERA_BASE + 0x08<<2)

#define VERA_DC_VIDEO     (VERA_BASE + 0x09<<2)
#define VERA_DC_HSCALE    (VERA_BASE + 0x0A<<2)
#define VERA_DC_VSCALE    (VERA_BASE + 0x0B<<2)
#define VERA_DC_BORDER    (VERA_BASE + 0x0C<<2)

#define VERA_DC_HSTART    (VERA_BASE + 0x09<<2)
#define VERA_DC_HSTOP     (VERA_BASE + 0x0A<<2)
#define VERA_DC_VSTART    (VERA_BASE + 0x0B<<2)
#define VERA_DC_VSTOP     (VERA_BASE + 0x0C<<2)

#define VERA_L0_CONFIG    (VERA_BASE + 0x0D<<2)
#define VERA_L0_MAPBASE   (VERA_BASE + 0x0E<<2)
#define VERA_L0_TILEBASE  (VERA_BASE + 0x0F<<2)
#define VERA_L0_HSCROLL_L (VERA_BASE + 0x10<<2)
#define VERA_L0_HSCROLL_H (VERA_BASE + 0x11<<2)
#define VERA_L0_VSCROLL_L (VERA_BASE + 0x12<<2)
#define VERA_L0_VSCROLL_H (VERA_BASE + 0x13<<2)

#define VERA_L1_CONFIG    (VERA_BASE + 0x14<<2)
#define VERA_L1_MAPBASE   (VERA_BASE + 0x15<<2)
#define VERA_L1_TILEBASE  (VERA_BASE + 0x16<<2)
#define VERA_L1_HSCROLL_L (VERA_BASE + 0x17<<2)
#define VERA_L1_HSCROLL_H (VERA_BASE + 0x18<<2)
#define VERA_L1_VSCROLL_L (VERA_BASE + 0x19<<2)
#define VERA_L1_VSCROLL_H (VERA_BASE + 0x1A<<2)

#define VERA_PALETTE_BASE  0x2000
#define VERA_SPRITES_BASE  0x1000

#define VERA_VRAM_BASE    0x40000

#endif //VERA_H