16 stack-create (compileto-stack)

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



