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

welcome
s" Ready." type cr
quit
