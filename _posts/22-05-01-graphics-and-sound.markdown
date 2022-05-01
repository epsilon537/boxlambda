---
layout: post
title: 'Key Components Part 2: Graphics and Sound Cores.'
comments: true
---

I spent some time researching graphics and sound options for BoxLambda. Here's what I came up with.

### Graphics

If you're reading this, you must be into the build-your-own-computer thing, which probably means you're aware of the super cool [Commander X16](https://www.commanderx16.com) project. Frank van de Hoef created the very elegant **VERA** (Video Embedded Retro Adapter) module for the X16. Here's a high-level specification, taken from the Commander X16 website:

VERA module specifications:

- Video generator featuring:
  - Multiple output formats (VGA, NTSC Composite, NTSC S-Video, RGB video) at a fixed resolution of 640x480@60Hz
  - Support for 2 layers, both supporting:
	- 1/2/4/8 bpp tile and bitmap modes
	- Support for up to 128 sprites (with inter-sprite collision detection).
  - Embedded video RAM of 128 KB.
  - Palette with 256 colors selected from a total range of 4096 colors.
- 16-channel stereo Programmable Sound Generator with multiple waveforms (Pulse, Sawtooth, Triangle, Noise)
- High-quality PCM audio playback from a 4 KB FIFO buffer featuring up to 48kHz 16-bit stereo sound.
- SecureDigital storage.

Other features, not mentioned in the blurb, include: 

- Fractional display scaling (scaling lower resolutions up to the 640x480 display resolution).
- Horizontal and Vertical smooth scrolling

Lucky for us, Frank recently released the VERA verilog code under the generous MIT license. You can find the code here: 

[https://github.com/fvdhoef/vera-module](https://github.com/fvdhoef/vera-module)

I'm not particularly interested in VERA's PSG (Programmable Sound Generator), or the non-VGA output formats, so I might remove those from the build.

The 128KB of video RAM will take a big chunk out of our available Block RAM resources, but it'll be worth it. We're getting a lot of bang for our buck.

Note that the VERA is designed as a separate FPGA with a SPI slave interface. Some modifications will be required to integrate it into our SoC.

#### Xosera

I also considered, but eventually dismissed, Xosera: 

[https://hackaday.io/project/173731-xosera-fpga-based-retro-video-graphics](https://hackaday.io/project/173731-xosera-fpga-based-retro-video-graphics). 

Xosera is a VERA-inspired video controller, but it is being developed independently by [Xarc](https://hackaday.io/Xark). I like the [Amiga-style Copper](https://en.wikipedia.org/wiki/Original_Chip_Set) processor that they added. Unfortunately, Xosera doesn't have hardware sprites. That's a showstopper for me. I'll keep my eye on this project though. It's an active project and features are still being added.

### Sound

A sound core is a perfect candidate for Partial FPGA Reconfiguration. There are a lot of options (Wave-Table synthesis, FM synthesis, PSG...) and a lot of open-source cores available. It would be pretty cool if the software application can just download its synthesizer of choice as part of the program.

Pretty much any core developed by [Jotego](https://github.com/jotego) sounds like a great idea.

Technically, I don't have to select a sound core. We already have sound through VERA's PCM audio playback. I'm going to select a sound core anyway because I like retro sounds and I'd like to mess around a bit with one of the old-school PSG chips. 

I think I'll go for a dual [**YM2149**](https://en.wikipedia.org/wiki/General_Instrument_AY-3-8910), one for music, one for sound FX, in a game context. The YM2149 was the Atari ST's sound chip, so we'll have a large music and sound FX archive at our disposal. Jotego developed an FPGA clone of the YM2149, the JT49:

[https://github.com/jotego/jt49](https://github.com/jotego/jt49)

#### Why not VERA PSG?
The only reason I'm not going for VERA PSG is that, as of yet, very little music has been written for it. I'm sure it is a perfectly adequate PSG implementation.

#### Why not SID?
The SID chip is partially analog, making it much harder to emulate correctly on an FPGA. Also, while I like SID, I've probably heard enough SID music to last me a lifetime. I'm currently more interested in finding out what other retro sound chips have to offer.

### Interesting Links

- [https://misterfpga.org/](https://misterfpga.org/) : Jotego is one of the key developers behind the MiSTer FGPA project.
- [https://www.youtube.com/watch?v=u5Mi3FkZgFI](https://www.youtube.com/watch?v=u5Mi3FkZgFI) : a compilation of some excellent YM2149 music made for the Atari ST by genius composer Scavenger (Joris de Man).
- [https://en.wikipedia.org/wiki/List_of_sound_chips](https://en.wikipedia.org/wiki/List_of_sound_chips) : A list of sound chips
