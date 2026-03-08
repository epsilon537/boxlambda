\
\ Lambdas
\

\ Begin lambda
: [: ( -- )
  state @ if
    \ [: is invoked as a compiling word, i.e. a code-generating word that
    \ executes when it's encountered in the definition of an other word.
    \ When [: _executes_...
    \ [: compiles an ahead, this pushes 2 items on the stack: ( patchaddr structmatchconst)
    postpone ahead
    \ [: puts here on the stack. This the entry point of the code ahead is skipping
    \ over: ( lambdaentry patchaddr structmatchconst )
    here -rot
    \ [: compiles a add sp, sp -4 sw ra, (sp), i.e. it generates a prologue.
    \ (lambdaentry patchaddr structmatchconst )
    postpone push_ra
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
  postpone exit \ ;] compiles an epilogue. ( lambdaentry patchaddr structmatchconst )
  dup 0= if \ a 0 structmatchconst means that we were in execution state when [: was entered.
    postpone [ ( lambdaentry patchaddr structmatchconst)
    2drop ( lambdaentry )
  else \ an ahead was compiled by [:
    postpone then \ ;] compiles a then matching the ahead and consuming the 2 stack items ahead produced. ( lambdaentry ).
    literal, \ ;] compiles the lambda entry point as a literal.
             \ when the literal executes (when the word invoking  [:..;] in its definition executes),
             \ the lambda entrypoint is pushed onto the stack.
  then
  [immediate] [compileonly]
;

\ The upshot of
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

