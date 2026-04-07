\ BoxLambda Forth
\ init.fs executes as soon as the BoxLambda OS has booted up to the
\ point that the include Word has been created, i.e. right after the
\ BoxKern has exeuted shell.fs.

\ Note that ifdef/else is defined here. Forth modules
\ earlier in the boot sequence such as fs.fs and shell.fs
\ (see sw/projects/boxlambda_os/main.cpp) don't have access to
\ ifdef/else.
include /forth/ifdef.fs

include /forth/disasm.fs
include /forth/dump.fs
include /forth/dict.fs

\ This flag is set when building the boxkerntest target.
[ifdef] FORTH_CORE_TEST
false include-verbose !
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

\ A quit loop that prints the cwd as a prompt
: quit_w_cwd ( -- )
  begin
    cr f_getcwd type s" > " type
    query
    cr
    interpret
  again
;

Flamingo cr
." Ready." cr

\ Update the quit hook so that if quit is called,
\ we get back into the quit_w_cwd loop.
' quit_w_cwd hook-quit !
quit_w_cwd

