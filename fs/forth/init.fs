\ BoxLambda Forth
\ init.fs executes after fastboot has completed or, in case of slowboot,
\ after evaulating slowboot-includes.fs.

\ enable run-time type checking
true xassert-enable !
true rttc-struct !

[ifdef] FORTH_CORE_TEST
true stack-checking-enable !
[then]

include /forth/vera.fs

vera import

[ifdef] FORTH_CORE_TEST
true include-verbose !
cd /test/vera/
.s cr
include vera-bitmap-1bpp.fs
.s cr
include vera-bitmap-2bpp.fs
.s cr
include vera-bitmap-4bpp.fs
.s cr
include vera-bitmap-8bpp.fs
.s cr
include vera-bitmap-paloffset.fs
.s cr
include vera-tile-1bpp.fs
.s cr
include vera-tile-2-4-8bpp.fs
.s cr
include vera-sprite-pixels.fs
.s cr
include vera-map-corners-256.fs
.s cr
include vera-map-test.fs
.s cr
include vera-layers.fs
.s cr
include vera-mapentry-tile.fs
.s cr
include vera-mapentry-txt16.fs
.s cr
include vera-mapentry-txt256.fs
.s cr
include vera-map-corners.fs
.s cr
include vera-map-test-cont.fs
.s cr
include vera-palette-test.fs
.s cr
include vera-palette-sys-colors.fs
.s cr
include vera-scale.fs
.s cr
include vera-scanline.fs
.s cr
include vera-screen-boundaries.fs
.s cr
include vera-scroll.fs
.s cr
include vera-sprite-bank.fs
.s cr
include vera-sprite-collision.fs
.s cr
include vera-sprite-first-last.fs
.s cr
include vera-sprite-hflip.fs
.s cr
include vera-sprite-info.fs
.s cr
include vera-sprite-paloffset.fs
.s cr
include vera-sprite-vflip.fs
.s cr
include vera-sprite-xy.fs
.s cr
include vera-sprite-z.fs
.s cr
include vera-tileset-params.fs
.s cr
include vera-stack-params.fs
.s cr
include vera-bitmap-tilesize.fs
.s cr
\ Run error test cases with stack-checking disabled.
\ It would get in the way of raised exceptions.
false stack-checking-enable !
include vera-bitmap-pos-err.fs
.s cr
include vera-bitmap-test-err.fs
.s cr
include vera-map-pos-err.fs
.s cr
include vera-map-test-err.fs
.s cr
include vera-sprite-params-err.fs
.s cr
include vera-tile-pos-err.fs
.s cr
include vera-tileset-params-err.fs
.s cr
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

