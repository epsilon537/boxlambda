\ BoxLambda Forth
\ init.fs executes after fastboot has completed or, in case of slowboot,
\ after evaulating slowboot-includes.fs.
\ point that the include Word has been created, i.e. right after the
\ BoxKern has exeuted shell.fs.

\ disable run-time type checking
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
include vera-bitmap-1bpp.fs
include vera-bitmap-2bpp.fs
include vera-bitmap-4bpp.fs
include vera-bitmap-8bpp.fs
include vera-bitmap-paloffset.fs
include vera-tile-1bpp.fs
include vera-tile-2-4-8bpp.fs
include vera-sprite-pixels.fs
include vera-map-corners-256.fs
include vera-map-test.fs
include vera-layers.fs
include vera-mapentry-tile.fs
include vera-mapentry-txt16.fs
include vera-mapentry-txt256.fs
include vera-map-corners.fs
include vera-map-test-cont.fs
include vera-palette-test.fs
include vera-palette-sys-colors.fs
include vera-scale.fs
include vera-scanline.fs
include vera-screen-boundaries.fs
include vera-scroll.fs
include vera-sprite-bank.fs
include vera-sprite-collision.fs
include vera-sprite-first-last.fs
include vera-sprite-hflip.fs
include vera-sprite-info.fs
include vera-sprite-paloffset.fs
include vera-sprite-vflip.fs
include vera-sprite-xy.fs
include vera-sprite-z.fs
include vera-tileset-params.fs
include vera-stack-params.fs
include vera-bitmap-tilesize.fs
\ Run error test cases with stack-checking disabled.
\ It would get in the way of raised exceptions.
false stack-checking-enable !
include vera-bitmap-pos-err.fs
include vera-bitmap-test-err.fs
include vera-map-pos-err.fs
include vera-map-test-err.fs
include vera-sprite-params-err.fs
include vera-tile-pos-err.fs
include vera-tileset-params-err.fs
quit
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

