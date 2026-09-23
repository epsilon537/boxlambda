\ BoxLambda Forth
\
\ Simple stack object with cell-size elements.

: x-stack-obj-overflow ( -- ) ." stack object overflow" cr ;
: x-stack-obj-underflow ( -- ) ." stack object underflow" cr ;

\ The stack object structure:
\ --------
\ 0: stack-top -------------------|
\ 1: stack-end ---------------------|
\ 2: stack-base: stack slot 0     | |
\ 3:             stack slot 1     | |
\ ...                           <-| |
\ n+1:           stack slot n-1     |
\ --------                       <--|
begin-structure stack-struct
  field: .stack-top \ Pointer to next free slot. Increments as items are pushed.
  field: .stack-end \ Pointer to cell past end of space allocated to stack. Top must not go past this.
  field: .stack-base \ Not a pointer. The stack slots start here. Top must not go below this address.
end-structure

\ Create an empty stack object of n elements.
( n "name" -- )
: stack-create
  create here >r 2 + cells allot ( R: stack ) \ Add 2 cells for stack-top and stack-end.
  here r@ .stack-end ! ( R: stack )
  r@ .stack-base r> .stack-top ! \ Stack is empty when stack-top == stack-base.
;

\ Push x on the stack.
\ ( x stack -- )
: stack-push
  >r ( x R: stack )
  r@ .stack-top @ ( x sp R: stack )
  dup r@ .stack-end @ < averts x-stack-obj-overflow ( x sp R: stack )
  tuck ! ( sp R: stack )
  cell+ r> .stack-top ! 
;

\ Pop x from the stack.
\ ( stack -- x )
: stack-pop
  >r ( R: stack )
  r@ .stack-top @ ( sp R: stack )
  dup r@ .stack-base > averts x-stack-obj-underflow ( sp R: stack )
  cell - dup r> .stack-top ! ( sp )
  @ ( x )
;

\ Number of elements currently on the stack
( stack -- n )
: stack-depth
  dup .stack-top @ swap .stack-base - 2 rshift
;

\ Return the maximum number of elements on the stack.
( stack -- n )
: stack-max-depth
  dup .stack-end @ swap .stack-base - 2 rshift
;

\ Returns the number of free elements left on the stack.
( stack -- n )
: stack-free
  dup .stack-end @ swap .stack-top @ - 2 rshift
;

\ Return the base address of the stack
( stack -- stack-base-addr )
: stack-base .stack-base ;

\ Returns the address of the next free element on the stack.
\ Note that this may point one cell past the end of the stack max. depth.
( stack -- stack-top-addr )
: stack-top .stack-top @ ;

\ Set the stack top to the given address.
\ E.g. <stack> dup stack-base swap >stack-top resets the stack.
\ Raises x-stack-obj-underflow or x-stack-obj-overflow if given address is 
\ outside the stack object range.
( addr stack -- )
: >stack-top
  2dup .stack-base >= averts x-stack-obj-underflow
  2dup .stack-end @ < averts x-stack-obj-overflow
  .stack-top !
;

\ Reset the stack.
( stack -- )
: stack-reset
  dup stack-base ( stack addr ) 
  swap >stack-top 
;

\ Find x in the stack and return its address or 0 if not found.
( x stack -- addr|0 )
: stack-find
  dup .stack-base >r ( x stack R: stack-base )
  .stack-top @ ( x stack-ptr R: stack-base )
  begin
    cell -
    dup r@ >= while ( x stack-ptr R: stack-base )
      2dup @ = if ( x stack-ptr R: stack-base )
        rdrop nip exit ( stack-ptr )
      then
  repeat
  rdrop 2drop 0
;

