16 stack-create (compileto-stack)

hook-on-quit @ variable compileto-prev-on-quit-hook

: compileto-on-quit-hook
  \ Reset the stack on quit
  (compileto-stack) stack-base (compileto-stack) >stack-top
  compileto-prev-on-quit-hook @ execute
;

' compileto-on-quit-hook hook-on-quit !

\ Save the current compileto state (compiletoimem/emem)
( -- )
: compileto-save
  compileto> (compileto-stack) stack-push
;

\ Restore the compileto state
( -- )
: compileto-restore
  (compileto-stack) stack-pop >compileto
;



