\ BoxLambda Forth
\
\ Simple queue object with cell-size elements.

: x-queue-obj-overflow ( -- ) ." queue object overflow" cr ;
: x-queue-obj-underflow ( -- ) ." queue object underflow" cr ;

\ The queue object structure:
\ --------
\ 0: queue-oldest---------------|
\ 1: queue-newest-----------------|
\ 2: queue-end ---------------------|
\ 3: queue-base: queue slot 0   | | |
\ 4:             queue slot 1 <-| | |
\ ...                           <-| |
\ n+2:           queue slot n-1     |
\ --------                       <--|
begin-structure queue-struct
  field: .queue-oldest \ Pointer to the oldest item in the queue. Increments as items are popped.
  field: .queue-newest \ Pointer to next free slot. Increments as items are pushed.
  field: .queue-end \ Pointer to cell past end of space allocated to queue.
  field: .queue-base \ Not a pointer. The queue slots start here.
end-structure

\ Create an empty queue object of n elements.
( n "name" -- )
: queue-create
  [ 1 0 stack-checker ]
  create here >r ( n R: queue )
  \ n elements starting at queue-base and room for the other struct fields.
  1- cells queue-struct + allot ( R: queue )
  here r@ .queue-end ! ( R: queue )
  r@ .queue-base r@ .queue-oldest !
  r@ .queue-base r> .queue-newest ! \ queue is empty when oldest == newest.
;

( queue ptr -- ptr' )
: (queueptr-wrapinc)
  [ 2 1 stack-checker ]
  cell+ ( queue ptr' )
  2dup swap .queue-end @ = if ( queue ptr' )
    drop
    dup .queue-base
  then ( queue ptr' )
  nip ( ptr' )
;

\ ( queue -- f )
: queue-empty?
  [ 1 1 stack-checker ]
  dup .queue-oldest @ ( queue oldest )
  swap .queue-newest @ ( oldest newest )
  =
;

\ ( queue -- f )
: queue-full?
  [ 1 1 stack-checker ]
  dup .queue-newest @ ( queue newest )
  over swap (queueptr-wrapinc) ( queue newest' )
  swap .queue-oldest @ = ( f )
;

\ Push x on the queue. May raise x-queue-obj-overflow.
\ ( x queue -- )
: queue-push
  [ 2 0 stack-checker ]
  dup queue-full? triggers x-queue-obj-overflow ( x queue )
  >r ( x R: queue )
  r@ .queue-newest @ ( x newest R: queue )
  tuck ! ( newest R: queue )
  r@ swap (queueptr-wrapinc) ( newest' R: queue)
  r> .queue-newest ! ( )
;

\ Pop x from the queue. May raise x-queue-obj-underflow.
\ ( queue -- x )
: queue-pop
  [ 1 1 stack-checker ]
  dup queue-empty? triggers x-queue-obj-underflow ( queue )
  >r ( R: queue )
  r@ .queue-oldest @ ( oldest R: queue )
  dup @ ( oldest x R: queue )
  r@ rot ( x queue oldest R: queue )
  (queueptr-wrapinc) ( x oldest' R: queue )
  r> .queue-oldest ! ( x )
;

\ Return the maximum number of elements on the queue
( queue -- n )
: queue-max-depth
  [ 1 1 stack-checker ]
  dup .queue-end @ ( queue end )
  swap .queue-base - ( end-base )
  2 rshift 1-
;

\ Number of elements currently on the queue
( queue -- n )
: queue-depth
  [ 1 1 stack-checker ]
  dup .queue-newest @ ( queue newest )
  over .queue-oldest @ ( queue newest oldest )
  2dup >= if ( queue newest oldest )
    - 2 rshift ( queue depth )
    nip ( depth )
  else ( queue newest oldest )
    rot queue-max-depth 1+ 2 lshift ( newest oldest qsize )
    - - 2 rshift ( depth )
  then
;

\ Returns the number of free elements left on the queue.
( queue -- n )
: queue-#free
  [ 1 1 stack-checker ]
  dup queue-max-depth ( queue max )
  swap queue-depth ( max depth )
  - ( free )
;

\ Reset the queue.
( queue -- )
: queue-reset
  [ 1 0 stack-checker ]
  dup .queue-base over .queue-oldest !
  dup .queue-base over .queue-newest !
  drop
;

