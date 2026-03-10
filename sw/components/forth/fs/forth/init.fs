include forth/ifdef.fs
include forth/disasm.fs
include forth/dump.fs
include forth/dict.fs

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
    f_getcwd type s" > " type
    query
    interpret
    cr
  again
;

welcome
s" Ready." type cr

' quit_w_cwd hook-quit !
quit_w_cwd

