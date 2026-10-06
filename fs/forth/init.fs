\ BoxLambda Forth
\ init.fs executes after fastboot has completed or, in case of slowboot,
\ after evaulating slowboot-includes.fs.

vera import
vera-init

[ifdef] FORTH_CORE_TEST
true include-verbose !
cd /test/vera/
include vera-bitmap-1bpp.fs
vram.
include vera-bitmap-2bpp.fs
vram.
include vera-bitmap-4bpp.fs
vram.
include vera-bitmap-8bpp.fs
vram.
include vera-bitmap-pal-group.fs
vram.
include vera-tile-1bpp.fs
vram.
include vera-tile-2-4-8bpp.fs
vram.
include vera-sprite-pixels.fs
vram.
include vera-map-corners-256.fs
vram.
include vera-map-test.fs
vram.
include vera-layers.fs
vram.
include vera-mapentry-tile.fs
vram.
include vera-mapentry-txt16.fs
vram.
include vera-mapentry-txt256.fs
vram.
include vera-map-corners.fs
vram.
include vera-map-test-cont.fs
vram.
include vera-palette-test.fs
vram.
include vera-pal-group-test.fs
vram.
include vera-palette-sys-colors.fs
vram.
include vera-scale.fs
vram.
include vera-scanline.fs
vram.
include vera-screen-boundaries.fs
vram.
include vera-scroll.fs
vram.
include vera-sprite-bank.fs
vram.
include vera-sprite-collision.fs
vram.
include vera-sprite-first-last.fs
vram.
include vera-sprite-hflip.fs
vram.
include vera-sprite-info.fs
vram.
include vera-sprite-pal-group.fs
vram.
include vera-sprite-vflip.fs
vram.
include vera-sprite-xy.fs
vram.
include vera-sprite-z.fs
vram.
include vera-tileset-params.fs
vram.
include vera-stack-params.fs
vram.
include vera-bitmap-size.fs
vram.
\ Run error test cases with stack-checking disabled.
\ It would get in the way of raised exceptions.
false stack-checking-enable !
include vera-bitmap-pos-err.fs
vram.
include vera-bitmap-test-err.fs
vram.
include vera-map-pos-err.fs
vram.
include vera-map-test-err.fs
vram.
include vera-sprite-params-err.fs
vram.
include vera-tile-pos-err.fs
vram.
include vera-tileset-params-err.fs
vram.
cd /
include /test/testsuite.fs
[then]

\ a:f's flamingo as a the welcome message.
: Flamingo cr
  ."      _" cr
  ."     ^-)" cr
  ."      (.._          .._" cr
  ."       \`\\        (\`\\        (" cr
  ."        |>         ) |>        |)" cr
  ." ______/|________ (7 |` ______\|/_______a:f" cr
;

: welcome ( -- )
  cr
  Flamingo
  cr
;

Flamingo cr
." Ready." cr

quit_w_cwd

