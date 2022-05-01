---
layout: post
title: 'Key Components Part 3: DMA and Peripherals.'
comments: true
---

Let's wrap up the selection of key components for the BoxLambda computer.

### DMA

I was on the fence for a while deciding whether or not I should include a DMA engine in our machine. We could just get by without one. A DMA core is by definition a bus master and having multiple bus masters (DMA and CPU) adds significant complexity to the architecture: access to shared buses and slaves, impact on timing, etc. In a system with only one bus master, the CPU, you don't have to worry about any of that.
Then I remembered that BoxLambda is intended to be a platform for RTL experimentation. It would be silly to restrict these RTL experiments to bus slave components only. In other words, the BoxLambda architecture is going to have to accommodate bus masters, so we might as well include a DMA engine, if only as a proof of concept.

Use cases for DMA in the scope of our computer include:

- Moving data between external (DDR) and internal (Block RAM) memory.
- Streaming from memory to the audio DAC.
- Blitting, i.e. copying data into video memory, taking into account the video memory's organization. For instance, copying a rectangular block of data into a frame buffer requires striding between rows of pixel data. Another example: Bit planes with 1, 2, or 4 bits-per-pixel color depths require barrel shifting when copying data to a specific pixel offset.

I didn't find many DMA cores online. One reasonable option I did find, was the Wishbone DMA Controller from FreeCores: [https://github.com/freecores/wb_dma](https://github.com/freecores/wb_dma)

This controller supports two Wishbone ports, multiple DMA channels, command lists, circular buffers, FIFOs, incrementing, and non-incrementing modes, so there is a lot to like. It's not perfect though: Transfers are restricted to 32-bit word-aligned units. There's also no concept of strides between bursts, although that can be emulated to some degree by a command list.
The number of channels and features are parameterized. We should be able to use this core as-is. However, it won't be very useful as a blitter in its current form. Possibly, we can extend the implementation with blitter features later on.

### Storage

I'm going to use ZipCPU's SD Card Controller in combination with the FatFs software library to mount a FAT filesystem on the SD card:

- SD Card Controller: [https://github.com/ZipCPU/sdspi](https://github.com/ZipCPU/sdspi)
- FatFs library: [http://elm-chan.org/fsw/ff/00index_e.html](http://elm-chan.org/fsw/ff/00index_e.html)

The SD Card Controller has a Wishbone slave port.

### Keyboard and Mouse

FreeCores has PS/2 keyboard and mouse modules: [https://github.com/freecores/ps2](https://github.com/freecores/ps2)

These cores don't have a Wishbone slave port, so we're going to have to add that ourselves.

Note that the Nexys A7 has a USB HID host interface for keyboard and mouse which, with the help of clever firmware on a PIC24 microcontroller, presents itself to the FPGA as a PS/2 interface. See the [Nexys A7 Reference Manual](https://digilent.com/reference/programmable-logic/nexys-a7/reference-manual) for more details.

### I2C

The I2C interface can be used to hook up a [Real-Time Clock PMOD](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/) as well as a [Wii Nunchuck Adapter](https://www.reichelt.com/be/en/arduino-8211-wiichuck-nunchuck-adapter-ard-wii-nunchuck-p282673.html?CCOUNTRY=661&LANGUAGE=nl&GROUPID=9020&START=0&OFFSET=16&SID=93757c8e4582e90848068d74dbb71d4a2c938ebd13432dc6b9c96&LANGUAGE=EN&&r=1).

ZipCPU has an I2C core with a Wishbone port: [https://github.com/ZipCPU/wbi2c](https://github.com/ZipCPU/wbi2c).

### Serial Port

ZipCPU comes to the rescue once again with a UART implementation with a Wishbone interface: [https://github.com/ZipCPU/wbuart32](https://github.com/ZipCPU/wbuart32)

I think that completes the list of Key Components for the time being. More components (e.g. IRQ controller, PIT timers, GPIO) may come into the picture as we get deeper into the architecture or design phase.

Interesting Links
-----------------
[Amiga HRM, Blitter Section](http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0118.html): An in-depth description of Amiga's Blitter. A good example of how well-designed the Commodore Amiga was.
