\ BoxLambda Forth
\ Exception Handling example

: x-spc-exception ." Don't press space!" ;

: foo-no-catch
  ." Press any key except space." cr key bl = triggers x-spc-exception
  ." Thank you for not pressing space" cr
;

: foo-w-catch
  [: ." Press any key except space." cr key bl = triggers x-spc-exception ;] try
  ?dup if
    ." Exception caught. Rethrowing..." cr
    ?raise
  else
    ." Thank you for not pressing space. " cr
  then
;

