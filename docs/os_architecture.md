# OS Architecture

[![BoxLambda OS
Architecture.](assets/BoxLambda_OS_Architecture.png)](assets/BoxLambda_OS_Architecture.png)

*BoxLambda OS Architecture Block Diagram.*

The two main subsystems are the **BoxLambda Kernel** (**BoxKern**) and the
**Mecrisp Forth Environment**.

The BoxKern image is stored in flash memory. The Bootloader loads the kernel
into EMEM, then transfers control to it. The kernel's *CRT0* startup code then
further unpacks specific sections into EMEM and IMEM. (See
[here](sw_bootloader.md) for a more detailed description of the boot
sequence).

The Mercrisp Forth Environment lives as Forth source code on the SD card
filesystem.

## The BoxKern

The BoxLambda Kernel is subdivided into two cores:

- The **Mecrisp Forth Core**, written in RISC-V assembly language.
- The **C Core** containing C/C++ drivers, libraries and C runtime.

The BoxKern is mostly about leveraging existing non-Forth code. Most of the
BoxKern components already exist in BoxLambda's code base, the exceptions being:
- **The Foreign Function Interface (FFI)**: This is the mechanism used to
call Forth from C code and C from Forth code. BoxKern drivers and libraries
export their functionality to the Forth environments through the FFI.
- **The Console Driver**: In the current BoxLambda code base, Picolibc's *stdin*
and *stdout* are associated with the serial port. This I/O method has to be
replaced by a Console Driver which forwards output to and takes input from the
Forth-side REPL.

### The Mecrisp Forth Core

The Mecrisp Forth Core is written in RISC-V assembly language. It contains Forth
start-up code and the definitions of key Forth words, such as
interpreter/compiler words, that make up the core of the Forth system. After
booting the Mecrisp Forth Core, you have a small, operational Forth system.

I'm adding Filesystem Access (*FS Access*) words to the set of original Mecrisp
core words so that after booting the core, the remaining Forth words can be
loaded as Forth *.fs* source code files from the SD card filesystem. The
filesystem access words are based on those found in Peter Schmid's [Mecrisp
Cube](https://github.com/spyren/Mecrisp-Cube/blob/master/sdcard/man/FileSystem.md)
project.

## The Mecrisp Forth Environment

[![
Flamingo.](assets/flamingo.png)](assets/flamingo.png)
*Mecrisp Forth's Welcome message - ASCII art from Adreas Freise.*

Most *new* code will be written in Forth or in assembly language in the Forth
environment. Not everything in the Forth environment is new, still-
to-be-developed software, however. Some modules, such as the RISC-V assembler
and disassembler, are part of the Mecrisp Forth Quintus distribution.

### The Canvas REPL / Editor

Instead of a line-oriented prompt, I would like to provide a REPL that allows
you to move freely all over the screen, as is the case on the Commodore 64, for
instance. The **Canvas REPL** can perform double duty as a text editor.

[![The Canvas REPL.](assets/canvas_repl.png)](assets/canvas_repl.png)

*The Canvas REPL.*

Note that the REPL takes its input from the USB HID keyboards and sends it
output to the attached VGA display. There is no serial port terminal.

### The BoxKern Proxies

The *BoxKern Proxies* are Forth-side proxies of BoxKern drivers and libraries. The
proxies access the BoxKern drivers/libs through the FFI.

## The Extras

The greyed-out parts in the block diagram are components that are not essential
to create a working system. These components are likely added later (some
possibly never). This is just a crude binary partitioning of what really
is an [iterative spiraling
approach](https://epsilon537.github.io/boxlambda/hello-world/). I start from
something small but working (e.g., the Mecrisp Forth core running in a serial port
terminal) and keep growing it. The greyed-out parts are among the parts to be
added last.

### Binary Executables and ELF Loader

Two such extra components are the *Binary Executables* and *ELF
Loader*. I would like to have a way to extend the non-Forth part of the system
without making the extension part of the kernel monolith. Lua or uLisp could be
added to the system in such a way, for example. These executables can
communicate with the rest of the system through the FFI.

# The Display

The 640x480 display is organized into two layers. The top layer is an 80x60
character grid displaying the Canvas REPL. The bottom layer is a 2-bit-per-pixel
640x480 bitmap. The *GFX Primitives* module will render to this layer by
default.

