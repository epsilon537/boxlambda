---
hide:
  - toc
---

# The uLisp Test Build (WIP)

**uLisp**: [http://www.ulisp.com](http://www.ulisp.com)

**The software project directory**: [sw/projects/ulisp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ulisp)

This is an experimental BoxLambda port of uLisp. The port is still at a very
early stage. Currently, the uLisp build boots and provides access to the [VERA
Stack](sw_comp_vera_stack.md) as a uLisp extension.

# Building and Running uLisp on FPGA

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 1000000 8N1**.

Flash the `boxlambda_base` release bitstream onto the target:

```
cd binaries
flash_gw.sh arty_a7_100t boxlambda_base.bit
```

Or, as an alternative, build `boxlambda_base` from source and flash that build onto
the target:

```
cd build/arty-a7-100/gw/projects/boxlambda_base
make boxlambda_base.bit
make boxlambda_base_flash_gw
```

Build the uLisp software project:

```
cd build/arty-a7-100/sw/projects/ulisp
make ulisp
make ulisp_flash_sw
```

You should see the following output in the terminal:

```
BoxLambda first stage bootloader
--------------------------------
Version: v0.2.1
Initializing SDRAM...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
  m0, b00: |00000000000000000000000000000000| delays: -
  m0, b01: |11111111111111111111111111111000| delays: 14+-14
  m0, b02: |00000000000000000000000000000000| delays: -
  m0, b03: |00000000000000000000000000000000| delays: -
  m0, b04: |00000000000000000000000000000000| delays: -
  m0, b05: |00000000000000000000000000000000| delays: -
  m0, b06: |00000000000000000000000000000000| delays: -
  m0, b07: |00000000000000000000000000000000| delays: -
  best: m0, b01 delays: 14+-14
  m1, b00: |00000000000000000000000000000000| delays: -
  m1, b01: |11111111111111111111111111111100| delays: 14+-14
  m1, b02: |00000000000000000000000000000000| delays: -
  m1, b03: |00000000000000000000000000000000| delays: -
  m1, b04: |00000000000000000000000000000000| delays: -
  m1, b05: |00000000000000000000000000000000| delays: -
  m1, b06: |00000000000000000000000000000000| delays: -
  m1, b07: |00000000000000000000000000000000| delays: -
  best: m1, b01 delays: 14+-14
Switching SDRAM to hardware control.
Done.
Installing second stage bootloader in DDR...
Done.
Proceeding to boot stage 2...
Bootloader stage 2:
-------------------
Copying SW image from Flash to IMEM...
Done.
Starting SW image...
uLisp 4.7
495045>
```

To see a list of available VERA functions in uLisp, enter the following:

```
495045> (? vera)
Vera graphics API. General form: (vera :<action> [:<param keyword> [param value]] ...

(vera :init)
(vera :map <id> :width <w> :height <h> :map-type <t>)
(vera :map <id> :deinit)
(vera :map <id> :entry :x <x> :y <y> [:val <v>])
(vera :map <id> [:info])
(vera :tileset <id> :width <w> :height <h> :bpp <b> :num-tiles <n>)
(vera :tileset <id> :deinit)
(vera :tileset <id> [:info])
(vera :pixel :tileset <id> :tile <idx> :x <x> :y <y> [:val <v>])
(vera :linecapture :enable|:disable|:enabled)
(vera :linecapture :pixel :x <x>)
(vera :spritebank :select <idx>)
(vera :spritebank :selected)
(vera :ien :set|:clr :mask <m>)
(vera :ien :get)
(vera :isr :set :mask <m>)
(vera :isr :get)
(vera :irqline [<l>])
(vera :scanline)
(vera :display :enable|:disable|:enabled)
(vera :sprites :enable|:disable|:enabled)
(vera [:hscale <h>] [:vscale <v>])
(vera :hscale|:vscale)
(vera :bordercolor [<c>])
(vera :boundaries [:hstart <hs> :hstop <he> :vstart <vs> :vstop <ve>])
(vera :layer <id> :enable|:disable|:enabled)
(vera :layer <id> [:hscroll <h>] [:vscroll <v>])
(vera :layer <id> :hscroll|:vscroll)
(vera :layer <id> :map [<m>] [:tileset t])
(vera :layer <id> :tileset [<t>] [:map <m>])
(vera :layer <id> :bitmap [:tileset <t> :tile <i>])
(vera :layer <id> :pal-offset [<o>])
(vera :palette :restore)
(vera :palette <id> [:r <r> :g <g> :b <b>])
(vera :sprite <id> :init)
(vera :sprite <id> <sprite-attribute settings>)
  Where <sprite-attribute settings> is one or more of the following:
  :tileset <t> :tile <i>
  :x <x> / :y <y> / :z <z> / :pal-offset <o> / :mask <mask>
  :hflip :enable|:disable / :vflip :enable|:disable
(vera :sprite <id> <sprite-attribute>)
  Where <sprite-attribute> is one of the following:
  :tileset :x :y :z :pal-offset :mask :hflip :vflip
(vera :sprite <id> [:info])

495045>
```

## The uLisp Test Suite

[sw/projects/ulisp/test](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ulisp/test) contains a suite of uLisp test scripts used to verify the integration of BoxLambda features into uLisp.

The test suite can be run under the control of a host PC using the `lisp_test.py` Python module. `Lisp_test.py` assumes that the target boots into uLisp. The Python module interacts with the target via the serial port (a screen session) and openFPGAloader (to trigger reboots between scripts).

To run all testcases:

```
cd sw/projects/ulisp/test
python3
import lisp_test
lisp_test.all()
```

To run testcases separately:

```
lisp_test.test('<lisp script filename without .lisp extension>')
```

### Reference output files

`Lisp_test.py` checks the test script's output against a reference output file.
The output is expected to match exactly, except for *big numbers* (e.g.
addresses), which are replaced with XXXXXX strings. The reference output files
are located in the same directory as the test scripts and have the same file stems as their corresponding test scripts.

Here is an example reference output file:

```
start

XXXXXX> (vera :init)
nil

XXXXXX> (dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 320 32) (cons 640 16)))
      (dolist (bpp (list 1 2 4 8))
         (vera :tileset idx :width (car wh) :height (cdr wh) :bpp bpp :num-tiles
 1)
         (print (vera :tileset idx :info))
         (setq idx (1+ idx)))))
  (dotimes (ii 8)
    (vera :tileset ii :deinit)))

(:addr XXXXXXXXX :width 320 :height 32 :bpp 1 :num-tiles 1 :tilesize-bytes 1280)

(:addr XXXXXXXXX :width 320 :height 32 :bpp 2 :num-tiles 1 :tilesize-bytes 2560)

(:addr XXXXXXXXX :width 320 :height 32 :bpp 4 :num-tiles 1 :tilesize-bytes 5120)

(:addr XXXXXXXXX :width 320 :height 32 :bpp 8 :num-tiles 1 :tilesize-bytes 10240
)
(:addr XXXXXXXXX :width 640 :height 16 :bpp 1 :num-tiles 1 :tilesize-bytes 1280)

(:addr XXXXXXXXX :width 640 :height 16 :bpp 2 :num-tiles 1 :tilesize-bytes 2560)

(:addr XXXXXXXXX :width 640 :height 16 :bpp 4 :num-tiles 1 :tilesize-bytes 5120)

(:addr XXXXXXXXX :width 640 :height 16 :bpp 8 :num-tiles 1 :tilesize-bytes 10240
)
(:addr XXXXXXXXX :width 320 :height 32 :bpp 1 :num-tiles 1 :tilesize-bytes 1280)

(:addr XXXXXXXXX :width 320 :height 32 :bpp 2 :num-tiles 1 :tilesize-bytes 2560)

(:addr XXXXXXXXX :width 320 :height 32 :bpp 4 :num-tiles 1 :tilesize-bytes 5120)

(:addr XXXXXXXXX :width 320 :height 32 :bpp 8 :num-tiles 1 :tilesize-bytes 10240
)
(:addr XXXXXXXXX :width 640 :height 16 :bpp 1 :num-tiles 1 :tilesize-bytes 1280)

(:addr XXXXXXXXX :width 640 :height 16 :bpp 2 :num-tiles 1 :tilesize-bytes 2560)

(:addr XXXXXXXXX :width 640 :height 16 :bpp 4 :num-tiles 1 :tilesize-bytes 5120)

(:addr XXXXXXXXX :width 640 :height 16 :bpp 8 :num-tiles 1 :tilesize-bytes 10240
)
nil

XXXXXX>

'end
end
```

When creating new test cases, I execute the candidate test script using
`lisp_test.py` without providing a reference output file. I then check the
output of the executed script. If I like what I see, I execute
`lisp_test.save()` in the Python shell. This saves the previously run test script's output as the reference file for this script.

