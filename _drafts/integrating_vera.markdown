---
layout: post
title: 'Integrating VERA.'
comments: true
---

![VERA in the BoxLambda Architecture.](../assets/Arch_Diagram_VERA_focus.drawio.png)

*VERA in the BoxLambda Architecture.*

Recap
-----
This is a summary of the current state of affairs for BoxLambda. We have:
- An Ibex RISC-V core, Wishbone shared bus, timer, two GPIO ports, UART core, and internal memory.
- DDR3 external memory access through the Litex Memory Controller.
- OpenOCD-based Debug Access, both on FPGA and Verilator.
- Test builds running on Arty-A7-35T, Arty-A7-100T, and Verilator.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- A Linux CMake and Bender-based RTL build system.
- Automated testing on Verilator. 

In my [previous post](https://epsilon537.github.io/boxlambda/understanding-vera/), I analyzed the internal structure and operation of the VERA (*Versatile Embedded Retro Adapter*) graphics pipeline. In this post, I focus on the modifications needed to make the VERA core a suitable component for a 32-bit Wishbone-based SoC. Specifically, BoxLambda.

VERA Modifications
------------------
The [original VERA implementation](https://github.com/fvdhoef/vera-module) is a standalone FPGA controlled by an 8-bit microprocessor via a low-speed 8-bit external bus. Clearly, certain modifications in the VERA code base are necessary to be able to fit the VERA core as a component into BoxLambda's SoC. The subsections below highlight the implemented changes.

50MHz Core Clock Freqency
=========================
BoxLambda's system clock frequency is 50MHz. It would be convenient to run the VERA core at that same speed. As it turns out, that works just fine. I didn't run into any timing closure issues when synthesizing the core at 50MHz. For those parts of the core that actually need to run at the 25MHz pixel clock frequency (*composer*, *video_vga*), I introduced a toggling clock enable signal ([clock enables are recommended over clock dividers](https://electronics.stackexchange.com/questions/222972/advantage-of-clock-enable-over-clock-division)).

Running the VERA core at 50MHz has the additional benefit of doubling the bandwidth of the VRAM bus. That'll come in handy to absorb the impact of the VRAM scheduling and Sprite Rendering changes introduced below.

Wishbone Interface
==================
The 8-bit asynchronous external bus is replaced with a pipelined Wishbone slave interface. The interface has a 32-bit data port, 4 byte lane enables, and a 17-bit word addressed address port.

Generic Single Port RAM
=======================
The original VERA VRAM code uses a memory primitive, *SP256K*, that is specific to Lattice FPGA devices.
I replaced the SP256K instances with instances of a generic Single Port RAM module that, in theory, should work well across a whole range of FPGA devices. Having said that, I have only been testing the modifications on Verilator and Arty A7.

Configurable VRAM Size
======================
BoxLambda currently supports two FPGA configurations: The Arty-A7-35T and the Arty-A7-100T. I have to reduce VERA's VRAM size to 64KB to be able to fit it into the small memory footprint of the Arty-A7-35T, but I want to leave it at 128KB for the bigger Arty-A7-100T. To handle this, I added a **VRAM_SIZE_BYTES** parameter the top-level module and propagated it down to *main_ram_generic*, the module where the memory is instantiated.

The maximum value of *VRAM_SIZE_BYTES* is 131072 (128K). I have only tested values 65536 (64K) and 131072. 65536 is the default value for the Arty-A7-35T and for Verilator. 131072 is the default for Arty-A7-100T.

Memory Mapped Access to VRAM, Palette RAM and Sprite Attribute RAM
==================================================================
The original VERA VRAM access method is geared towards an external 8-bit microcontroller. This pretty much made indirect access (with benefits such as auto-increment/decrement) a necessity.
The wide address range of the Wishbone interface, combined with BoxLambda's 32-bit RISCV processor removes this necessity. In such a configuration, it makes more sense to offer a straightforward memory mapped interface to VRAM, Palette RAM, and Sprite Attribute RAM.

I removed the indirect memory access methods and created the following memory mapped address ranges:

| Address range           | Description       | Access     |
| ----------------------- | ----------------- | ---------- |
| 0x10100000 - 0x10100100 | VERA Registers    | Read/Write |
| 0x10101000 - 0x10101400 | Sprite attributes | Write Only |
| 0x10102000 - 0x10102200 | Palette           | Write Only |
| 0x10140000 - 0x10160000 | Video RAM (128KB) | Read/Write |

Note:
- The above addresses are absolute addresses, as seen by the RISCV processor. The VERA core's base address is 0x10100000.
- The Video RAM address range depends on the amount of Video RAM set by the *VRAM_SIZE_BYTES* macro. The range 0x10140000-0x10160000 corresponds to a *VRAM_SIZE_BYTES* setting of 131072 (128K).

Time Slot Scheduled Access to VRAM
==================================
There are four ports accessing VRAM: two Layer Renderers, the Sprite Renderer, and the CPU. The original VERA code uses a priority scheduler to decide which port gets access when two or more ports are competing for access. The CPU port had highest priority, then the Layer Renderers, and finally the Sprite Renderer. However, the high-speed, memory-mapped Wishbone interface makes it all too easy for the CPU to oversubscribe the VRAM bus and starve the other ports of bandwidth. This would result in tearing artifacts and other rendering errors. To avoid this issue, the scheduler is modified so that each port in turn gets a timeslot to access VRAM. There are four equal time slot *beats*. Each port is assigned to one slot. 

![Time Slot Scheduled VRAM Access.](../assets/vram_if_timeslot_scheduling.drawio.png)

*Time Slot Scheduled VRAM Access.*

With this mechanism, bandwidth utilization on one port does not have any impact on any of the other ports. A port that tries to use more than its share of the bus bandwidth is stalled. In practice, the only port where this can happen is the CPU port.

Sprite Banks
============
One nice consequence of the time slot scheduling is that the Sprites-per-Scanline limit no longer depends on the selected Layer Renderer modes. However, that limit also depends to some extent on the position of the enabled sprites in the Sprite Attribute Table. Each scanline, the Sprite Attribute Table is scanned front-to-back for enabled sprites. This takes time. As a result, if all enabled sprites are located towards the end of the table, the Sprites-per-Scanline limit is lower compared to having the same number of enabled sprites near the front of the table. This is undesirable. It goes against the grain of BoxLambda's [*deterministic behavior* requirement](https://epsilon537.github.io/boxlambda/requirements-analysis/).

One way to avoid the issue is to conservatively reduce the Sprite ID maximum value to the Sprites-per-Scanline limit minus 1, i.e. when the limit is reached, the table is full and there is no front-of-table or back-of-table effect to consider. By running the VERA core at 50MHz, the largest number of sprites that can be guaranteed to be rendered completely on one scanline is 64. This is for 8-pixel-wide sprites. In other words, the highest Sprites-per-Scanline limit is 64 and Sprite IDs should be limited to the range 0 to 63. 

However, limiting the Sprite ID maximum value to 63 would mean that the upper half of the Sprite Attributes RAM is left unused. We could just cut the size of that RAM in half. Instead, I chose to create two banks of 64 sprite IDs. A bit in the *VERA_CTRL* register is used to select the active bank. 

Sprite Banking may help with sprite multiplexing or animation: While one sprite bank is active, software can prepare the inactive bank's entries and switch over at the right moment, triggered by a *line_irq*, for instance.

![Double Buffering with Sprite Banks.](../assets/sprite_banking_double_buffering.drawio.png)

*Double Buffering with Sprite Banks.*

![Sprite Multiplexing with Sprite Banks.](../assets/sprite_banking_muxing.drawio.png)

*Sprite Multiplexing with Sprite Banks.*

A Fixed Sprite-Pixels-per-Scanline Limit
========================================
The Sprites-per-Scanline limit is inversely proportional to the sprite width. That makes semse. It takes roughly twice as long to render an 16-pixel-wide sprite than an 8-pixel-wide sprite (See also *Maximum number of Sprites per Scanline* table in the [Understanding VERA](https://epsilon537.github.io/boxlambda/understanding-vera/) post). Conversely, the number of *sprite pixels* that can be rendered on a given scanline is relatively constant. With all of the above modifications implemented, this constant is 512 pixels, i.e. the Sprite Renderer can render a maximum of 512 sprite pixel on any scanline, guaranteed.

The original Sprite Renderer code kept track of rendering time to decide when to abort rendering, to avoid exceeding its time budget. I replaced that code with logic that keeps track of the number of sprite pixels rendered. When 512 sprite pixels are rendered, further sprite rendering is aborted for the given scanline.

![512 Sprite Pixel Limit Examples.](../assets/sprite_pixel_limit_examples.drawio.png)

*Examples of the 512 Sprite Pixel Limit.*

The above changes combined make it easier for a programmer to plan sprites. The programmer knows ahead of time exactly how many sprites of a given size he can have on the same scanline. This number is independent of the Layer Renderer settings and the VRAM loading by the CPU (or any other Wishbone bus master accessing VRAM, such as a DMA core).

32-bit friendly register access
===============================
I reshuffled the various bitfields in VERA's register space for convenient access by a 32-bit CPU. Here is the modified register interface:

<table>
	<tr>
		<th>Addr</th>
		<th>Name</th>
		<th>31-12</th>
		<th>11</th>
		<th>10</th>
		<th>9</th>
		<th>8</th>
		<th>7</th>
		<th>6</th>
		<th>5 </th>
		<th>4</th>
		<th>3 </th>
		<th>2</th>
		<th>1 </th>
		<th>0</th>
	</tr>
	<tr>
		<td>$00</td>
		<td>CTRL</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">SBNK</td>
	</tr>
	<tr>
		<td>$04</td>
		<td>DC_BORDER</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="8" align="center">Border Color</td>
	</tr>
	<tr>
		<td>$08</td>
		<td>IEN</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">SPRCOL</td>
		<td colspan="1" align="center">LINE</td>
		<td colspan="1" align="center">VSYNC</td>
	</tr>
	<tr>
		<td>$0C</td>
		<td>ISR</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="4" align="center">Sprite collissions</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">SPRCOL</td>
		<td colspan="1" align="center">LINE</td>
		<td colspan="1" align="center">VSYNC</td>
	</tr>
	<tr>
		<td>$10</td>
		<td>IRQLINE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="10" align="center">IRQ line</td>
	</tr>
	<tr>
		<td>$14</td>
		<td>SCANLINE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="10" align="center">Scan line</td>
	</tr>
	<tr>
		<td>$18</td>
		<td>DC_VIDEO</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">Sprites Enable</td>
		<td colspan="1" align="center">Layer1 Enable</td>
		<td colspan="1" align="center">Layer0 Enable</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="2" align="center">Output Mode</td>
	</tr>
	<tr>
		<td>$20</td>
		<td>DC_HSCALE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="8" align="center">Active Display H-Scale</td>
	</tr>
	<tr>
		<td>$24</td>
		<td>DC_VSCALE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="8" align="center">Active Display V-Scale</td>
	</tr>
	<tr>
		<td>$28</td>
		<td>DC_HSTART</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="10" align="center">Active Display H-Start</td>
	</tr>
	<tr>
		<td>$2C</td>
		<td>DC_HSTOP</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="10" align="center">Active Display H-Stop</td>
	</tr>
	<tr>
		<td>$30</td>
		<td>DC_VSTART</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="10" align="center">Active Display V-Start</td>
	</tr>
	<tr>
		<td>$34</td>
		<td>DC_VSTOP</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="10" align="center">Active Display V-Stop</td>
	</tr>
	<tr>
		<td>$40</td>
		<td>L0_CONFIG</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="2" align="center">Map Height</td>
		<td colspan="2" align="center">Map Width</td>
		<td colspan="1" align="center">T256C</td>
		<td colspan="1" align="center">Bitmap Mode</td>
		<td colspan="2" align="center">Color Depth</td>
	</tr>
	<tr>
		<td>$44</td>
		<td>L0_MAPBASE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="8" align="center">Map Base Address (16:9)</td>
	</tr>
	<tr>
		<td>$48</td>
		<td>L0_TILEBASE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="6" align="center">Tile Base Address (16:11)</td>
		<td colspan="1" align="center">Tile Height</td>
		<td colspan="1" align="center">Tile Width</td>
	</tr>
	<tr>
		<td>$50</td>
		<td>L0_HSCROLL</td>
		<td colspan="1" align="center">-</td>
		<td colspan="12" align="center">H-Scroll</td>
	</tr>
	<tr>
		<td>$54</td>
		<td>L0_VSCROLL</td>
		<td colspan="1" align="center">-</td>
		<td colspan="12" align="center">V-Scroll</td>
	</tr>
	<tr>
		<td>$80</td>
		<td>L1_CONFIG</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="2" align="center">Map Height</td>
		<td colspan="2" align="center">Map Width</td>
		<td colspan="1" align="center">T256C</td>
		<td colspan="1" align="center">Bitmap Mode</td>
		<td colspan="2" align="center">Color Depth</td>
	</tr>
	<tr>
		<td>$84</td>
		<td>L1_MAPBASE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="8" align="center">Map Base Address (16:9)</td>
	</tr>
	<tr>
		<td>$88</td>
		<td>L1_TILEBASE</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="1" align="center">-</td>
		<td colspan="6" align="center">Tile Base Address (16:11)</td>
		<td colspan="1" align="center">Tile Height</td>
		<td colspan="1" align="center">Tile Width</td>
	</tr>
	<tr>
		<td>$90</td>
		<td>L1_HSCROLL</td>
		<td colspan="1" align="center">-</td>
		<td colspan="12" align="center">H-Scroll</td>
	</tr>
	<tr>
		<td>$94</td>
		<td>L1_VSCROLL</td>
		<td colspan="1" align="center">-</td>
		<td colspan="12" align="center">V-Scroll</td>
	</tr>
</table>

All registers are 32-bit wide, but higher order bits 31-12 are currently not in use.

Revised VERA Block Diagram and Feature Summary
----------------------------------------------

![The Revised VERA Block Diagram.](../assets/vera_wishbone.drawio.png)

*The Revised VERA Block Diagram.*

The revised VERA repository is called **vera_wishbone**:

[https://github.com/epsilon537/vera_wishbone](https://github.com/epsilon537/vera_wishbone)

VERA Wishbone's feature summary:
  - 32-bit pipelined Wishbone slave interface.
  - VGA output format at a fixed resolution of 640x480@60Hz (same as original VERA).
  - Support for 2 layers, both supporting either tile or bitmap mode (same as original VERA).
  - Support for 2 banks of 64 sprites, max. 512 sprite pixels per scanline.
  - Configurable Embedded video RAM size of up to 128kB.
  - Palette with 256 colors selected from a total range of 4096 colors (same as original VERA).

The Top-Level Module Interface
------------------------------
The *vera_wishbone* top-level interface is straightforward:

```
module vera_top #(
	parameter VRAM_SIZE_BYTES=(128*1024) //Max. 128KB. Tested sizes are 64K and 128K.
	)
  (
  input  wire       clk,
  input  wire       reset,

  //32-bit pipelined Wishbone interface.
  input wire [16:0]  wb_adr,
  input wire [31:0]  wb_dat_w,
  output wire [31:0] wb_dat_r,
  input wire [3:0]   wb_sel,
  output wire        wb_stall,
  input wire         wb_cyc,
  input wire         wb_stb,
  output wire        wb_ack,
  input wire         wb_we,
  output wire        wb_err,

  // IRQ
  output wire        irq_n,

  // VGA interface
  output reg  [3:0]  vga_r,       
  output reg  [3:0]  vga_g,       
  output reg  [3:0]  vga_b,       
  output reg         vga_hsync,   
  output reg         vga_vsync   
  );
```

The *vera_standalone* Test Project
----------------------------------
The *vera_standalone* test project introduced in the previous post is still functional. It's a Verilator simulation project containing just the VERA core and a test bench. The test bench has been updated to track the various modifications to the VERA core and it has been extended to support the rendering of the VGA output in a window using SDL.

Vera_standalone served its purpose of testing the VERA core and modifications pre-integration. Once the VERA core was integrated into the BoxLambda SoC, however, the **vera_integrated** test SoC project took over. I don't intend to further maintain the *vera_standalone* project after I have applied the *vera_integrated* git label. 

Location of the vera_standalone project: [boxlambda/gw/projects/vera_standalone](https://github.com/epsilon537/boxlambda/tree/3a38c2e6d7a57d87b97e18f65e14e60132390efc/gw/projects/vera_standalone).

The *vera_integrated* Test Project
----------------------------------
[boxlandba/gw/projects/vera_integrated](https://github.com/epsilon537/boxlambda/tree/3a38c2e6d7a57d87b97e18f65e14e60132390efc/gw/projects/vera_integrated) contains a BoxLambda test SoC including the *vera_wishbone* core. As is the case with the *vera_standalone* build, the associated test bench will render the VGA output to a window using SDL. The test bench will also save a fully rendered frame as a file, so it can be compared against a reference frame for automated testing.

In the *vera_integrated* build, VERA is configured by a test program running on the Ibex RISCV processor that's part of the test SoC. The test program communicates with the test bench over UART. This allows the test program to send test results to the test bench for the purpose of automated testing. The test program focuses on the VERA core modifications discussed above:
- Wishbone read and write access to VERA registers and memories.
- Sprite Banking.
- The 512 sprite-pixels-per-scanline limit.
- The modified register interface.
- VRAM bus timeslot scheduling by verifying that Layer and Sprite Rendering are not affected by CPU load on VRAM.

For the Arty A7 build, the *.xdc* constraints file used by *vera_integrated* assumes that [Diligent's VGA PMOD](https://digilent.com/reference/pmod/pmodvga/start) is being used, i.e. the VGA signals are sent to PMOD ports JB and JC with the following pin layout:

| Pin       | Description | Pin       | Description |
| --------- | ----------- | --------- | ----------- |
| JB Pin 1  | R0          | JC Pin 1  | G0          |
| JB Pin 2  | R1          | JC Pin 2  | G1          |
| JB Pin 3  | R2          | JC Pin 3  | G2          |
| JB Pin 4  | R3          | JC Pin 4  | G3          |
| JB Pin 5  | GND         | JC Pin 5  | GND         |
| JB Pin 6  | VCC         | JC Pin 6  | VC3V3       |
| JB Pin 7  | B0          | JC Pin 7  | HS          |
| JB Pin 8  | B1          | JC Pin 8  | VS          |
| JB Pin 9  | B2          | JC Pin 9  | NC          |
| JB Pin 10 | B3          | JC Pin 10 | NC          |
| JB Pin 11 | GND         | JC Pin 11 | GND         |
| JB Pin 12 | VCC3V3      | JC Pin 12 | VCC         |

Try It Out
----------
Setup
=====
1. Install the [Prerequisites](https://boxlambda.readthedocs.io/en/latest/installation-and-test-builds/#prerequisites). 
1. Get the BoxLambda repository:
	```
	git clone https://github.com/epsilon537/boxlambda/
	cd boxlambda
	```
1. Switch to the *vera_integrated* tag: 
	```
	git checkout vera_integrated
	```
1. Set up the repository. This initializes the git submodules used and creates the default build trees: 
	```
	./boxlambda_setup.sh
	```

Vera_standalone on Verilator
============================
1. Build the vera_standalone project:
	```
	cd build/sim/gw/projects/vera_standalone
	make vera_standalone_sim
	```
1. Execute the generated verilator model:
	```
	./Vmodel
	```
1. Vmodel opens a windows in which the VGA output is rendered pixel-by-pixel. You should see the following frame appear:

![Frame Generated by vera_standalone Vmodel.](../assets/vera_standalone_frame.png)

*Frame generated by vera_standalone Verilator model.*

Vera_integrated on Verilator
============================
1. Build the vera_integrated project:
	```
	cd build/sim/gw/projects/vera_integrated
	make vera_integrated_sim
	```
1. Execute the generated verilator model:
	```
	./Vmodel
	```
1. Vmodel opens a window where the VGA output is rendered pixel-by-pixel. You should see the following frame appear:

![Frame Generated by vera_integrated Vmodel.](../assets/vera_integrated_frame.png)

*Frame generated by vera_integrated Verilator model.*

  The terminal window should look like this:

![vera_integrated terminal window.](../assets/vera_integrated_terminal_window.png)

*Vera_integrated Verilator model terminal window output.*

Vera_integrated on the Arty A7
================================
1. If you're running on WSL, check BoxLambda's documentation [On WSL](https://boxlambda.readthedocs.io/en/latest/installation-and-test-builds/#on-wsl) section.
1. Hook up a VGA display to the Arty A7 PMOD ports JB and JC using [Diligent's VGA PMOD](https://digilent.com/reference/pmod/pmodvga/start).
7. Build the vera_integrated project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
	```
	cd build/arty-a7-35/gw/projects/vera_integrated
	make vera_integrated_impl
	```
1. Download the generated bitstream file to the Arty A7:
	```
	make vera_integrated_load
	```
1. The display should now show a bunch of colored squares. Here's a picture of my setup. Apologies for potato quality. Clearly, I'm spending more on FPGA dev boards than on cameras.
   
![Arty A7 Setup for vera_integrated Test SoC.](../assets/vera_fpga_test_setup.jpg)

*Arty A7 Setup for vera_integrated Test SoC.*

Conclusion
----------
Having taken the time to study the original VERA's internal structure and operation, identifying the necessary modifications needed to integrate the core into a 32-bit SoC was relatively straightforward. Most changes are implemented in the top-level module, *vera_top*, where the Wishbone bus is terminated, the VERA registers are kept, and the memory mapped access to VRAM, Palette RAM, and Sprite RAM are implemented. Key to the whole excercise is the Verilator simulation model of the VERA core and its ability to render the VGA output in a window.

In the next post, I would like to bring up the SD SPI controller and get a filesystem going. Thank you for reading. Please let me know what you think.
