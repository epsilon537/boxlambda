\ BoxLambda Forth
\ init.fs executes as soon as the BoxLambda OS has booted up to the
\ point that the include Word has been created, i.e. right after the
\ BoxKern has exeuted shell.fs.

\ A quit loop that prints the cwd as a prompt
: quit_w_cwd ( -- )
  begin
    cr f_getcwd type s" > " type
    query
    cr
    interpret
  again
;

\ Update the quit hook so that if quit is called,
\ we get back into the quit_w_cwd loop.
' quit_w_cwd hook-quit !

\ Note that ifdef/else is defined here. Forth modules
\ earlier in the boot sequence such as fs.fs and shell.fs
\ (see sw/projects/boxlambda_os/main.cpp) don't have access to
\ ifdef/else.
include /forth/ifdef.fs

[ifdef] FORTH_CORE_TEST
create RTTC \ Enable run-time type checking
[then]

include /forth/rttc-struct.fs
include /forth/disasm.fs
include /forth/dump.fs
include /forth/pre-dict.fs
include /forth/wordlist.fs
include /forth/dict.fs
include /forth/run-time-checking.fs
include /forth/module.fs
include /forth/bitfield.fs
include /forth/memmap.fs
include /forth/vera_regs.fs
include /forth/vera_spr_attr_ram.fs
include /forth/vec2.fs
include /forth/iter.fs

[ifdef] FORTH_CORE_TEST
true stack-checking-enable !
[then]

include /forth/vera.fs

vera import

\ This flag is set when building the boxkerntest target.
[ifdef] FORTH_CORE_TEST
true include-verbose !
cd /test/vera/
include vera-sprite-pixels-w8.fs
quit
include vera-bitmap-1bpp.fs
include vera-bitmap-2bpp.fs
include vera-bitmap-4bpp.fs
include vera-bitmap-8bpp.fs
include vera-bitmap-paloffset.fs
include vera-bitmap-pos-err.fs
include vera-bitmap-test-err.fs
include vera-tile-1bpp.fs
include vera-tile-2-4-8bpp.fs
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

