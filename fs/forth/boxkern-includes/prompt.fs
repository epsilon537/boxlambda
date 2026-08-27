\ A quit loop that prints the cwd as a prompt
: quit_w_cwd ( -- )
  \ Invoked on-quit hook to invoke custom clean-ups
  \ before entering loop.
  hook-on-quit @ execute

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


