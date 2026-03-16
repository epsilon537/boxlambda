include forth/disasm.fs
include forth/dump.fs
include forth/ifdef.fs
include forth/dict.fs
[ifdef] FORTH_CORE_TEST
true include-verbose !
include test/testsuite.fs
[then]

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
: quit_w_cwd
  begin
    cr f_getcwd type s" > " type
    query
    cr
    interpret
  again
;

welcome
s" Ready." type cr

' quit_w_cwd hook-quit !
quit_w_cwd

