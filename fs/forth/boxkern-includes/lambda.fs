\ BoxLambda Forth
\
\ Lambdas / Anonymous functions.
\ The [: ;] notation is borrowed from ZeptoForth. The implementation is BoxLambda
\ specific, however.
\

\ Begin lambda
: [: ( -- )
  state @ if
    \ [: is invoked as a compiling word, i.e. a code-generating word that
    \ executes when it's encountered in the definition of an other word.
    \ When [: _executes_... it compiles an 'ahead'. This ahead pushes
    \ 2 items on the stack: patchaddr and structmatchconst
    postpone ahead ( patchaddr structmatchconst )
    \ [: puts 'here' on the stack. This the entry point of the code that 'ahead' is skipping
    \ over.
    here -rot ( lambdaentry patchaddr structmatchconst )
    \ [: compiles an 'add sp, sp -4 sw ra, (sp)', i.e. it generates a prologue.
    postpone push_ra ( lambdaentry patchaddr structmatchconst )
  else
    \ [: is invoked while in execution state.
    ] \ Enter compilation state and push following 3 items on the stack for ;] to consume.
    here 0 0 \ ( lambdaentry patchaddr structmatchconst )
    postpone push_ra
  then
  [immediate]
;

\ End lambda
: ;] ( -- )
  \ When ;] _executes_...
  postpone exit ( lambdaentry patchaddr structmatchconst ) \ ;] compiles an epilogue...
  dup 0= if \ a 0 structmatchconst means that we were in execution state when [: was entered.
    \ ...compiles a switch-to-execution-state...
    postpone [ ( lambdaentry patchaddr structmatchconst)
    2drop ( lambdaentry )
  else \ an 'ahead' was compiled by [:
    postpone then ( lambdaentry ) \ ...compiles a 'then' matching the ahead and consuming the 2 stack items ahead produced...
    literal, \ ...compiles the lambda entry point as a literal.
             \ when the literal executes (when the word invoking  [:..;] in its definition executes),
             \ the lambda entrypoint is pushed onto the stack.
  then
  [immediate] [compileonly]
;

\ So, the end result of
\ ... [: <lambdadef> ;] ... is:
\        ...
\        ahead
\ xt: prologue
\        <lambdadef>
\        epilogue
\ then
\ xt
\        ...
\ i.e. [: .. ;] produces an xt representing the instructions within the [: .. ;].

