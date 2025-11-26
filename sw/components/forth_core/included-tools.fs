const char included_tools[] =  R"included_tools(
\ -----------------------------------------------------------------------------
\   Conditional compilation
\ -----------------------------------------------------------------------------
\ ."  ok." added to nexttoken (for e4thcom compatibility) MM-201227

\ Idea similar to http://lars.nocrew.org/dpans/dpansa15.htm#A.15.6.2.2532

: nexttoken ( -- addr len )
  begin
    token          \ Fetch new token.
  dup 0= while      \ If length of token is zero, end of line is reached.
\   2drop cr query   \ Fetch new line.
    2drop ."  ok." cr query  \ Tell the terminal or user to send a next line.
  repeat
;

: [else] ( -- )
  1 \ Initial level of nesting
  begin
    nexttoken ( level addr len )

    2dup s" [if]"     compare
 >r 2dup s" [ifdef]"  compare r> or
 >r 2dup s" [ifndef]" compare r> or

    if
      2drop 1+  \ One more level of nesting
    else
      2dup s" [else]" compare
      if
        2drop 1- dup if 1+ then  \ Finished if [else] is reached in level 1. Skip [else] branch otherwise.
      else
        s" [then]" compare if 1- then  \ Level completed.
      then
    then

    ?dup 0=
  until

  immediate 0-foldable
;

: [then] ( -- ) immediate 0-foldable ;

: [if]   ( ? -- )                 0=  if postpone [else] then immediate 1-foldable ;
: [ifdef]  ( -- ) token find drop 0=  if postpone [else] then immediate 0-foldable ;
: [ifndef] ( -- ) token find drop 0<> if postpone [else] then immediate 0-foldable ;

\ RISC-V 32 IM Disassembler, Copyright (C) 2018  Matthias Koch
\ This is free software under GNU General Public License v3.
\ Usage: Specify your target address in disasm-$ and give disasm-step some calls.

\ ------------------------
\  A quick list of words
\ ------------------------

: list ( -- )
  cr
  dictionarystart
  begin
    dup 8 + ctype space
    dictionarynext
  until
  drop
;

\ ---------------------------------------
\  Memory pointer and instruction fetch
\ ---------------------------------------

0 variable disasm-$   \ Current position for disassembling

\ --------------------------------------------------
\  Try to find address as code start in Dictionary
\ --------------------------------------------------

: disasm-string ( -- ) \ Takes care of an inline string
  disasm-$ @ 4 + dup ctype skipstring 4 - disasm-$ !
;

: name. ( Address -- ) \ If the address is Code-Start of a dictionary word, it gets named.
  >r
  dictionarystart
  begin
    dup   8 + dup skipstring  r@ = if ."   --> " ctype else drop then
    dictionarynext
  until
  drop
  r>

  case \ Check for inline strings ! They are introduced by calls to ." or s" internals.
    ['] (.") of ."   -->  ." [char] " emit space disasm-string [char] " emit endof \ It is ." runtime ?
    ['] (s") of ."   -->  s" [char] " emit space disasm-string [char] " emit endof \ It is .s runtime ?
    ['] (c") of ."   -->  c" [char] " emit space disasm-string [char] " emit endof \ It is .c runtime ?
  endcase
;

\ -------------------
\  Beautiful output
\ -------------------

: u.2  0 <# # # #> type ;
: u.4  0 <# # # # # #> type ;
: u.8  0 <# # # # # # # # # #> type ;
: u.ns 0 <# #s #> type ;
: const. ."  #" u.ns ;
: addr. u.8 ;

: .decimal base @ >r decimal . r> base ! ;

: register. ( u -- )
  $1F and
  case
    0 of ." zero" endof
    dup ." x" decimal u.ns hex
  endcase ;

\ ----------------------------------------
\  Disassembler logic and opcode cutters
\ ----------------------------------------

: inst ( -- x ) disasm-$ @ @ ;

: funct3 ( -- x ) inst 12 rshift $07 and ;
: funct7 ( -- x ) inst 25 rshift ;

: .rs1 ( -- ) inst 15 rshift register. ;
: .rs2 ( -- ) inst 20 rshift register. ;
: .rd  ( -- ) inst  7 rshift register. ;

: imm_i  ( -- x ) inst 20 arshift ;

: imm_s  ( -- x ) inst $FE000000 and 20 arshift
                  inst 7 rshift $1F and or ;

: imm_sb ( -- x ) inst   31 12 - rshift   1 12 lshift and
                  inst   25  5 - rshift          $7E0 and   or
                  inst    8  1 - rshift           $1E and   or
                  inst   11  7 - lshift   1 11 lshift and   or
                  19 lshift 19 arshift ;

: imm_u  ( -- x ) inst $FFFFF000 and ;

: imm_uj ( -- x ) inst   31 20 - rshift   1 20 lshift and
                  inst   21  1 - rshift          $7FE and   or
                  inst   20 11 - rshift   1 11 lshift and   or
                  inst                         $FF000 and   or
                  11 lshift 11 arshift ;

\ ---------------
\  Disassembler
\ ---------------

0 variable disasm-destination

: disasm-load ( -- )

  funct3
  case
    0 of ." lb     " endof
    1 of ." lh     " endof
    2 of ." lw     " endof
    4 of ." lbu    " endof
    5 of ." lhu    " endof
  endcase

  .rd ." , " imm_i . ." (" .rs1 ." )"
;

: disasm-immediate ( -- )

  imm_i

  funct3
  case
    0 of ." addi   " endof
    1 of ." slli   " endof
    2 of ." slti   " endof
    3 of ." sltiu  " endof
    4 of ." xori   " dup disasm-destination xor! endof
    5 of inst 26 rshift 16 = if ." srai   " else ." srli   " then $1F and endof
    6 of ." ori    " endof
    7 of ." andi   " endof
  endcase

  .rd ." , " .rs1 ." , " .
;

: disasm-auipc ( -- )
  ." auipc  " .rd ." , " imm_u u.8
;

: disasm-store ( -- )

  funct3
  case
    0 of ." sb     " endof
    1 of ." sh     " endof
    2 of ." sw     " endof
  endcase

  .rs2 ." , " imm_s . ." (" .rs1 ." )"
;

: disasm-register ( -- )

  funct7 1 = \ Multiplication & Division RV32M
  if
    funct3
    case
      0 of ." mul    " endof
      1 of ." mulh   " endof
      2 of ." mulhsu " endof
      3 of ." mulhu  " endof
      4 of ." div    " endof
      5 of ." divu   " endof
      6 of ." rem    " endof
      7 of ." remu   " endof
    endcase
  else
    funct3
    case
      0 of funct7 32 = if ." sub    " else ." add    " then endof
      1 of ." sll    " endof
      2 of ." slt    " endof
      3 of ." sltu   " endof
      4 of ." xor    " endof
      5 of funct7 32 = if ." sra    " else ." srl    " then endof
      6 of ." or     " endof
      7 of ." and    " endof
    endcase
  then

  .rd ." , " .rs1 ." , " .rs2
;

: disasm-lui ( -- )
  ." lui    " .rd ." , " imm_u u.8
  imm_u disasm-destination !
;

: disasm-branch ( -- )

  funct3
  case
    0 of ." beq    " endof
    1 of ." bne    " endof
    4 of ." blt    " endof
    5 of ." bge    " endof
    6 of ." bltu   " endof
    7 of ." bgeu   " endof
  endcase

  .rs1 ." , " .rs2 ." , " disasm-$ @ imm_sb + u.8
;

: disasm-jalr ( -- )
  ." jalr   " .rd ." , " imm_i . ." (" .rs1 ." )"

  inst 15 rshift $1F and 15 = if disasm-destination @ imm_i + name. then
;

: disasm-jal
  ." jal    " .rd ." , " disasm-$ @ imm_uj + dup u.8 name.
;

: disasm-system ( -- )

  funct3
  ?dup
  if \ CSR...
  case

    1 of ." csrrw  " endof
    2 of ." csrrs  " endof
    3 of ." csrrc  " endof

    5 of ." csrrwi " endof
    6 of ." csrrsi " endof
    7 of ." csrrci " endof
  endcase

  .rd ." , "
  inst 20 rshift u.4  ." , "
  funct3 5 u< if .rs1 else inst 15 rshift $1F and u.2 then

  else \ PRIV
    inst 20 rshift

    case
      $000 of ." ecall"  endof
      $001 of ." ebreak" endof
      $105 of ." wfi"    endof
      $302 of ." mret"   endof

      ." Unknown system opcode"
    endcase
  then
;

: disasm ( -- ) \ Disassembles one machine instruction and advances disasm-$

    inst $7F and
    case
      $03 of disasm-load      endof
      $13 of disasm-immediate endof
      $17 of disasm-auipc     endof
      $23 of disasm-store     endof
      $33 of disasm-register  endof
      $37 of disasm-lui       endof
      $63 of disasm-branch    endof
      $67 of disasm-jalr      endof
      $6F of disasm-jal       endof
      $73 of disasm-system    endof

      ." Unknown opcode"
    endcase
    4 disasm-$ +!
;

\ ------------------------------
\  Single instruction printing
\ ------------------------------

: memstamp \ ( Addr -- ) Shows a memory location nicely
    dup u.8 ." : " @ u.8 ."   " ;

: disasm-step ( -- )
    disasm-$ @                 \ Note current position
    dup memstamp disasm cr     \ Disassemble one instruction

    begin \ Write out all disassembled memory locations
      4 + dup disasm-$ @ <>
    while
      dup memstamp cr
    repeat
    drop
;

\ ------------------------------
\  Disassembler for definitions
\ ------------------------------

: seec ( -- ) \ Continues to see
  base @ hex cr

  begin
    disasm-$ @ addrinimem?
    disasm-$ @ addrinemem?    or not
    disasm-$ @ @ $00008067 = or \ Flag: Loop terminates with ret or when leaving memory area.
    disasm-$ @ @ $30200073 = or \       Also check for mret
    disasm-step
  until

  base !
;

: see ( -- ) \ Takes name of definition and shows its contents from beginning to first ret
  ' disasm-$ !
  seec
;

\ =========================================================================
\  File: fixpt-math-lib.fs for Mecrisp-Stellaris by Matthias Koch
\
\  This file contains these functions for s31.32 fixed point numbers:
\
\           sqrt, sin, cos, tan, asin, acos, atan
\           log2, log10, ln, pow2, pow10, exp
\
\ ------------- Comments on sqrt and trig functions -----------------------
\
\  All angles are in degrees.
\
\  Accuracy is good rounded to 7 significant digits, with some exceptions.
\  In particular, the asin and acos functions have reduced accuracy
\  near the endpoints of the range of their inputs (+/-1) due to their
\  very large slopes there.  See the tests in fixpt-mat-lib-tests.fs.
\
\  The sin function is based on Maclaurin series for sin and cos over
\  the interval [0, pi/4], evaluated as polynomials with the Horner
\  method.  This is extended to all angles using (anti)symmetry.  Cos is
\  calculated from sin with cos(x) = sin(x+90), and tan is calculated as
\  sin/cos.
\
\  Atan is based on the first 7 terms of its Euler series over the
\  interval [0, 1/8].  It is extended to [0, 1] using the identity
\
\   atan(x) = atan(c) + atan((x-c)/(1+x*c)), c = 1/8, 2/8, ..., 7/8, 1.
\
\  For x>1 we use atan(x) = 90 - atan(1/x).  Negative arguments are handled
\  by antisymmetry.  Asin and acos are calculated using the formulas
\
\    asin(x) = atan(x / sqrt(1 - x^2)),  x^2 <= 1/2
\
\    asin(x) = 90 - atan(sqrt(1 - x^2) / x),  x^2 > 1/2
\
\    acos(x) = 90 - asin(x)
\
\  The square root is calculated bitwise with a standard algorithm over
\  the interval [0, 1] and is extended to all positive x by division by 4
\  until the quotient is in [0, 1].
\
\ ------------- Comments on the log and power functions -------------------
\
\  The user can check for accuracy by running the test functions in the
\  file fixpt-math-lib-tests.fs, or by running tests tailored to their use.
\  Generally the functions are accurate when rounded to about 7 significant
\  digits.  However, the user should not expect good accuracy when dealing
\  with very small fractional values due to the limitations of fixed
\  point.  In particular, this affects the values of the power and
\  exponential functions for larger negative inputs, when the relative
\  accuracy decreases significantly.
\
\  If the argument to a log function is non-positive, the function returns
\  "minus infinity," the largest negative s31.32 value.  This is the only
\  signal that an invalid input has been used.  Large negative inputs
\  to the power and exponential functions will return zero.  Large positive
\  inputs will return "plus infinity," the largest positive s31.32 value.
\  The code shows the specific values used to determine "large" in each
\  case.
\
\  The algorithm for calculating the base 2 log is taken from pseudocode
\  in the Wikipedia article "Binary logarithm" which is based on:
\
\     Majithia, J. C.; Levan, D. (1973), "A note on base-2 logarithm
\     computations", Proceedings of the IEEE, 61 (10): 1519–1520,
\     doi:10.1109/PROC.1973.9318
\
\  The log10(x) and natural logarithm ln(x) make use of the identities
\
\     log10(x) = log10(2)*log2(x), ln(x) = ln(2)*log2(x)
\
\  where log10(2) and ln(2) are given constants.
\
\  The pow2(x) = 2^x function is calculated as 2^x = (2^z)*(2^n) where
\  x = z + n, n is an integer with n <= x < n+1, and 0 <= z < 1.  The
\  factor 2^z is calculated by the identity 2^z = exp(ln(2)*z) where
\  exp(y) is calculated using its Maclaurin series.  The other factor
\  is accounted for by shifting 2^z n times (shift left for n > 0, shift
\  right for n < 0).
\
\  The pow10(x) = 10^x function is calculated using the identity
\
\               10^x = 2^(x*ln(10)/ln(2))
\
\  except for positive integer values of x, where simple multiplication
\  is used.
\
\  The exp(y) function is calculated using the series above if y is
\  between -0.36 and +0.36.  Otherwise it is calculated from pow2 using the
\  identity exp(y) = pow2(y/ln(2)).
\
\ -------------------------------------------------------------------------
\  Note:  Some s31.32 constant values were rounded from theoretical values
\         and entered below as (comma-part) integers rather calculating
\         them using Forth conversions, which trucate.
\
\ -------------------------------------------------------------------------
\  Andrew Palm
\  2018.04.09
\ =========================================================================


\ -------------------------------------------------------------------------
\  Misc. helper words, constants, and variables
\ -------------------------------------------------------------------------
\ Most positive and negative s31.32 values possible
$FFFFFFFF $7FFFFFFF 2constant +inf  \ 2147483647,9999999999
$0 $80000000 2constant -inf         \ 2147483648,0

\ Return the floor of an s31.32 value df
: floor ( df -- df ) nip 0 swap 2-foldable ;

\ Convert an s31.32 angle df1 in degrees to an angle df2 in [0, 360)
\ such that df1 = df2 + n*360 where n is an integer
: deg0to360 ( df1 -- df2 )  360,0 d/mod 2drop 2dup d0< if 360,0 d+ then
  2-foldable
;

\ Convert an s31.32 angle df1 in degrees to an angle df2 in [-90, 90)
\ such that df1 = df2 + n*180 where n is an integer.  (For tan only.)
: deg-90to90 ( df1 -- df2 )
  180,0 d/mod 2drop
  2dup 90,0 d< not if
    180,0 d-
  else
    2dup -90,0 d< if
      180,0 d+
    then
  then
  2-foldable
;

\ From common directory of Mecrisp-Stellaris Forth 2.4.0
: numbertable <builds does> swap 2 lshift + @ ;

\ -------------------------------------------------------------------------
\  Square root functions
\ -------------------------------------------------------------------------
: 0to1sqrt ( x -- sqrtx )
  \ Take square root of s31.32 number x with x in interval [0, 1]
  \ Special cases x = 0 and x = 1
  2dup d0= if exit then
  2dup 1,0 d= if exit then

  swap    \ Put x in MSW of unsigned 64-bit integer u

  \ Find square root of u as 64-bit unsigned int
  0,0 2swap 1,0 30 lshift  \ Stack: ( res  u  bit )
  \ Start value of bit is highest power of 4 <= u
  begin 2over 2over du< while dshr dshr repeat

  \ Do while bit not zero
  begin 2dup 0,0 d<> while
    2rot 2over 2over d+ 7 pick 7 pick du> not if  \ u >= res+bit ?
      2rot 2over d- 2rot 2tuck d-    \ u = u - res - bit
      2swap 2rot dshr 2over d+            \ res = (res >> 1) + bit
    else
      dshr    \ res = res >> 1
    then
    2-rot       \ Return stack to ( res u bit )
    dshr dshr   \ bit = bit >> 2
  repeat

  \ Drop u and bit, res is s31.32 square root of x
  2drop 2drop
  2-foldable
;

: sqrt ( x -- sqrtx )
  \ Find square root of non-negative s31.32 number x
  \ If x in interval [0, 1], use 0to1sqrt
  2dup 1,0 d> not if
    0to1sqrt
  else
    \ Divide x by 4 until result is in interval [0, 1]
    0,0 2swap   \ Init count of divides ndiv (use double for convenience)
    begin 2dup 1,0 d> while
      2swap 1,0 d+ 2swap    \ Incr count
      dshr dshr             \ Divide by 4
    repeat
    0to1sqrt
    2swap nip 0 do    \ ndiv consumed
      dshl                  \ Multiply by 2 ndiv times
    loop
  then
  2-foldable
;

\ -------------------------------------------------------------------------
\  Helpers and constants for trig functions
\ -------------------------------------------------------------------------
: deg2rad ( deg -- rad )
  \ Convert s31.32 in degress to s31.32 in radians
  74961321 0 f*
  2-foldable
;

: rad2deg ( rad -- deg )
  \ Convert s31.32 in radians to s31.32 in degrees
  1270363336 57 f*
  2-foldable
;

\ pi/2 and pi/4 as s31.32 numbers (whole part first for retrieval with 2@)
2451551556 1 2constant pi/2
3373259426 0 2constant pi/4

\ s31.32 comma parts of coefficients in Horner expression of 7-term series
\ expansion of sine after an x is factored out.  The whole parts are 0 and
\ are supplied in code.
numbertable sin-coef
   20452225 ,   \  1/(14*15)
   27531842 ,   \  1/(12*13)
   39045157 ,   \  1/(10*11)
   59652324 ,   \  1/(8*9)
  102261126 ,   \  1/(6*7)
  214748365 ,   \  1/(4*5)
  715827883 ,   \  1/(2*3)

: half-q1-sin-rad  ( x -- sinx )
  \ Sin(x) for x in first half of first quadrant Q1 and its negative
  \ x is a s31.32 angle in radians between -pi/4 and pi/4
  2dup 2dup f*          \  x and x^2 on stack as dfs
  \ Calculate Horner terms
  -1,0   \ Starting Horner term is -1
  7 0 do
    \ Multiply last term by x^2 and coefficient, then add +1 or -1 to get
    \ new term
    2over f* i sin-coef 0 f* 0 1
    i 2 mod 0= if d+ else d- then
  loop
  \ Last term is multiplied by x
  2nip f*
  2-foldable
;

\ s31.32 comma parts of coefficients in Horner expression of 8-term series
\ expansion of cosine.  The whole parts are 0 and are supplied in code.
numbertable cos-coef
   17895697 ,   \  1/(15*16)
   23598721 ,   \  1/(13*14)
   32537631 ,   \  1/(11*12)
   47721859 ,   \  1/(9*10)
   76695845 ,   \  1/(7*8)
  143165577 ,   \  1/(5*6)
  357913941 ,   \  1/(3*4)
 2147483648 ,   \  1/2

: half-q1-cos-rad  ( x -- cosx )
  \ Cos(x) for x in first half of first quadrant Q1 and its negative
  \ x is a s31.32 angle in radians between -pi/4 and pi/4
  2dup f*          \  x^2 on stack
  \ Calculate Horner terms
  1,0   \ Starting Horner term is 1
  8 0 do
    \ Multiply last term by x^2 and coefficient, then add +1 or -1 to get
    \ new term
    2over f* i cos-coef 0 f* 0 1
    i 2 mod 0= if d- else d+ then
  loop
  2nip
  2-foldable
;

: q1-sin-rad ( x -- sinx )
  \ Sin(x) for x in first quadrant Q1 and its negative
  \ x is a s31.32 angle in radians between -pi/2 and pi/2
  2dup pi/4 d< if
    half-q1-sin-rad
  else
    pi/2 2swap d- half-q1-cos-rad
  then
  \ Apply max/min limits
  \ 2dup 1,0 d> if 2drop 1,0 exit then
  \ 2dup -1,0 d< if 2drop -1,0 exit then
  2-foldable
;

: q1toq4-sin ( x -- sinx )
  \ Sin(x) for x in quadrants Q1 through Q4
  \ x is a s31.32 angle in degrees between 0 and 360
  2dup 270,0 d> if
    360,0 d- deg2rad q1-sin-rad
  else 2dup 90,0 d> if
    180,0 d- deg2rad q1-sin-rad dnegate
  else
    deg2rad q1-sin-rad
  then then
  2-foldable
;

\ s31.32 comma parts of coefficients in Horner expression of 6-term Euler
\ expansion of atan after x/(x^2+1) is factored out.  The whole parts are
\ 0 and are supplied in code.  The series variable is y = (x^2)/(x^2+1).
numbertable atan-coef
  3964585196 ,    \   12/13
  3904515724 ,    \   10/11
  3817748708 ,    \   8/9
  3681400539 ,    \   6/7
  3435973837 ,    \   4/5
  2863311531 ,    \   2/3

: base-ivl-atan ( x -- atanx )
  \ Calc atan for s32.31 x in base interval 0 to 1/8.
  2dup 2dup f* 2dup 1,0 d+     \ Stack: ( x  x^2  x^2+1 )
  2rot 2swap f/                \ Stack: ( x^2  x/(x^2+1) )
  2swap 2dup 1,0 d+ f/         \ Stack: ( x/(x^2+1)  (x^2)/(x^2+1) )
  \ Calc Horner terms for powers of y = (x^2)/(x^2+1)
  1,0   \ Starting Horner term is 1
  6 0 do
    \ Multiply last term by y and coefficient, then add 1 to get new term
    2over f* i atan-coef 0 f* 1,0 d+
  loop
  \ Last term is multiplied by x/(x^2+1)
  2nip f*
  2-foldable
;

\ Table of atan(i/8), i = 0, 1, ..., 8, values in radians
\ Only comma parts given, all whole parts are 0.
numbertable atan-table
           0 ,
   534100635 ,
  1052175346 ,
  1540908296 ,
  1991351318 ,
  2399165791 ,
  2763816217 ,
  3087351340 ,
  3373259426 ,

: 0to1-atan ( x -- atanx )
  \ Calc atan for s31.32 x in interval [0, 1]
  2dup 1,0 d= if
    2drop
    8 atan-table 0
  else
    \ Find interval [i/8, (i+1)/8) containing x, then use formula
    \ atan(x) = atan(i/8) + atan((x - (i/8))/(1 + (x*i/8))) where
    \ the argument in the second term is in [0, 1/8].
    0 7 do
      0 i 8,0 f/ 2over 2over d< not if
        2over 2over d-
        2-rot f* 1,0 d+
        f/ base-ivl-atan
        i atan-table 0 d+
        leave
      else
        2drop
      then
    -1 +loop
  then
  2-foldable
;

\ -------------------------------------------------------------------------
\  Trig functions
\ -------------------------------------------------------------------------
: sin ( x -- sinx )
  \ x is any s31.32 angle in degrees
  2dup 2dup d0< if dabs then
  \ Stack is ( x |x| )
  360,0 ud/mod 2drop
  q1toq4-sin    \ sin|x|
  \ Negate if x is negative
  2swap d0< if dnegate then
  2-foldable
;

: cos ( x -- cosx )
  \ x is any s31.32 angle in degrees
  90,0 d+ sin
  2-foldable
;

: tan ( x -- tanx )
  \ x is any s31.32 angle in degrees
  \ Move x to equivalent value in [-90, 90)
  deg-90to90
  \ If |x| > 89,9 deg, use approximation sgn(x)(180/pi)/(90-|x|)
  2dup dabs 2dup 89,8 d> if
    90,0 2swap d- 608135817 3 f* 180,0 2swap f/
    2swap d0< if dnegate then
  else
    2drop 2dup sin 2swap cos f/
  then
  2-foldable
;

: atan ( x -- atanx )
  \ Calc atan for s31.32 x, return result in degrees
  2dup 2dup d0< if dabs then   \ Stack: ( x |x| )
  \ Find atan(|x|)
  2dup 1,0 d> if
    \ |x| > 1, use atan(|x|) = (pi/2) - atan(1/|x|) with 1/|x| in [0, 1]
    1,0 2swap f/ 0to1-atan pi/2 2swap d-
  else
    \ |x| <= 1
    0to1-atan
  then
  \ Negate if x is negative
  2swap d0< if dnegate then
  rad2deg
  2-foldable
;

: asin ( x -- asinx )
  \ Calc asin for s31.32 x in interval [-1, 1], return result in degrees
  2dup 2dup d0< if dabs then
  \ Stack is ( x |x| )
  2dup 1,0 d> if drop exit then     \ Exit if |x|>1 with x on stack
  2dup 2dup f* 1,0 2swap d- 0to1sqrt    \ Stack: ( x  |x|  sqrt(1-x^2) )
  2over 2dup f* 0,5 d> if           \ x^2 > (1/2) ?
    2swap f/ atan 90,0 2swap d-
  else
    f/ atan
  then
  \ Negate if x is negative
  2swap d0< if dnegate then
  2-foldable
;

: acos ( x -- acosx )
  \ Calc acos for s31.32 x in interval [-1, 1], return result in degrees
  90,0 2swap asin d-
  2-foldable
;

\ -------------------------------------------------------------------------
\  Helper for logarithmic functions
\ -------------------------------------------------------------------------
: log2-1to2 ( y -- log2y )
  \ Helper function that requires y is s31.32 value with 1 <= y < 2
  0 0 2swap 0
  ( retval y cum_m )
  \ while((cum_m < 33) && (y > 1))
  begin dup 2over
    ( retval y cum_m cum_m y )
    1,0 d> swap 33 < and while
    ( retval y cum_m )
    rot rot 0 -rot        \ m = 0, z = y
    ( retval cum_m m z)
    \ Do z = z*z, m = m+1 until 2 <= z.  We also get z < 4
    begin
      2dup f* rot 1 + -rot
      ( retval cum_m m z )
      2dup 2,0 d< not
    until
    \ At this point z = y^(2^m) so that log2(y) = (2^(-m))*log2(z)
    \ = (2^(-m))*(1 + log2(z/2)) and 1 <= z/2 < 2
    \ We will add m to cum_m and add 2*(-cum_m) to the returned value,
    \ then iterate with a new y = z/2
    ( retval cum_m m z )
    2swap + -rot dshr 2>r   \ cum_m = cum_m + m, y = z/2
    ( retval cum_m ) ( R: y=z/2 )
    \ retval = retval + 2^-cum_m
    1,0 2 pick 0 do dshr loop
    ( retval cum_m 2^-cum_m )
    rot >r d+
    ( retval ) ( R: y cum_m )
    r> 2r> rot
    ( retval y cum_m )
  repeat
  drop 2drop
  2-foldable
;

\ -------------------------------------------------------------------------
\  Logarithmic functions
\ -------------------------------------------------------------------------
: log2 ( x -- log2x )
  \ Calculates base 2 logarithm of positive s31.32 value x

  \ Treat error and special cases
  \ Check that x > 0.  If not, return "minus infinity"
  2dup 0,0 d> not if 2drop -inf exit then
  \ If x = 1, return 0
  2dup 1,0 d= if 2drop 0,0 exit then

  \ Find the n such that 1 <= (2^(-n))*x < 2
  \ This n is the integer part (characteristic) of log2(x)
  0 -rot
  ( n=0 y=x )
  2dup 1,0 d> if
    \ Do n = n+1, y = y/2 while (y >= 2)
    begin 2dup 2,0 d< not while
      ( n y )
      dshr rot 1 + -rot
    repeat
  else
    \ Do n = n-1, y = 2*y while (y < 1)
    begin 2dup 1,0 d< while
      ( n y )
      dshl rot 1 - -rot
    repeat
  then

  \ Now y = (2^(-n))*x so log2(x) = n + log2(y) and we use the
  \ helper function to get log2(y) since 1 <= y < 2
  log2-1to2 rot 0 swap d+
  ( log2x )
  2-foldable
;

1292913986 0 2constant log10of2

: log10 ( x -- log10x )
  \ Calculates base 10 logarithm of positive s31.32 value x

  \ Treat error and special cases
  \ Check that x > 0.  If not, return "minus infinity"
  2dup 0,0 d> not if 2drop -inf exit then
  \ If x = 1, return 0
  2dup 1,0 d= if 2drop 0,0 exit then

  \ Find the n such that 1 <= (10^(-n))*x < 10
  \ This n is the integer part (characteristic) of log2(x)
  0 -rot
  ( n=0 y=x )
  2dup 1,0 d> if
    \ Do n = n+1, y = y/10 while (y >= 10)
    begin 2dup 10,0 d< not while
      ( n y )
      10,0 f/ rot 1 + -rot
    repeat
  else
    \ Do n = n-1, y = 10*y while (y < 1)
    begin 2dup 1,0 d<  while
      ( n y )
      10,0 f* rot 1 - -rot
    repeat
  then

  \ Now y = (10^(-n))*x so log10(x) = n + log10(y) and we use the
  \ identity log10(y) = log10(2)*log2(y)
  log2 log10of2 f* rot 0 swap d+
  ( log10x )
  2-foldable
;

2977044472 0 2constant lnof2

: ln ( x -- lnx )
  \ Return the natural logarithm of a postive s31.32 value x

  \ Treat error and special cases
  \ Check that x > 0.  If not, return "minus infinity"
  2dup 0,0 d> not if 2drop -inf exit then
  \ If x = 1, return 0
  2dup 1,0 d= if 2drop 0,0 exit then

  log2 lnof2 f*
  2-foldable
;

\ -------------------------------------------------------------------------
\  Power functions
\ -------------------------------------------------------------------------
\ s31.32 comma parts of all but first coefficient in Horner expansion of
\ a partial sum of the series expansion of exp(x).  The whole parts are 0
\ and are supplied in code.
numbertable exp-coef
   390451572 ,   \  1/11
   429496730 ,   \  1/10
   477218588 ,   \  1/9
   536870912 ,   \  1/8
   615366757 ,   \  1/7
   715827883 ,   \  1/6
   858993459 ,   \  1/5
  1073741824 ,   \  1/4
  1431655765 ,   \  1/3
  2147483648 ,   \  1/2

: exp-1to1 ( x -- expx )
  \ Calculate exp(x) for x an s31.32 value.  Values are correct when
  \ when rounded to six decimal places when x is between +/-0.7.  Uses an
  \ 11-term partial sum evaluated using Horner's method.
  \ Calculate Horner terms
  1,0   \ Starting Horner term is 1
  10 0 do
    \ Multiply last term by x and coefficient, then add to get new term
    2over f* i exp-coef 0 f* 0 1 d+
  loop
  \ Last part of expansion
  2over f* 0 1 d+
  2nip
  2-foldable
;

: pow2 ( x -- 2^x )
  \ Return 2 raised to the power x where x is s31.32
  \ If x is 0, return 1
  2dup 0,0 d= if 2drop 1,0 exit then
  \ If x < -32, 0 is returned.  If x >= 31, returns s31.32 ceiling
  2dup -32,0 d< if 2drop 0,0 exit then
  2dup 31,0 d< not if 2drop +inf exit then
  \ Get largest integer n such that n <= x so x = z + n, 0 <= z < 1
  2dup floor 2swap 2over d-
  ( n z )
  \ Get exp(z*ln2) = 2^z, then shift n times to get 2^x = (2^n)*(2^z)
  lnof2 f* exp-1to1 2swap nip
  ( 2^z n )  \ n now a single
  dup 0= if
    drop
  else
    dup 0< if
      negate 0 do dshr loop
    else
      0 do dshl loop
    then
  then
  2-foldable
;

1901360723 1 2constant 1overlnof2

: exp ( x -- expx )
  \ Return the exponential e^x of the s31.32 value x
  \ If x is 0, return 1
  2dup 0,0 d= if 2drop 1,0 exit then
  \ Return s31.32 ceiling for large pos. exponents, 0 for large neg.
  2dup 21,5 d> if 2drop +inf exit then
  2dup -22,2 d< if 2drop 0,0 exit then
  \ If |x| < 0.36, use exponential series approximation
  \ Otherwise, use exp(x) = pow2(x/ln(2))
  2dup dabs 0,36 d< if
    exp-1to1
  else
    1overlnof2 f* pow2
  then
  2-foldable
;

1382670639 3 2constant ln10overln2

: pow10 ( x -- 10^x )
  \ Return 10 raised to the power x where x is s31.32
  \ If x is 0, return 1
  2dup 0,0 d= if 2drop 1,0 exit then
  \ Return s31.32 ceiling for large pos. exponents, 0 for large neg.
  2dup 9,35 d> if 2drop +inf exit then
  2dup -9,64 d< if 2drop 0,0 exit then
  \ If x is a positive integer generate powers of 10 with multiplications
  \ Otherwise use 10^x = 2^(x*ln(10)/ln(2))
  2dup 2dup floor d= if
    2dup 0,0 d> if
      1,0 2swap nip
      0 do 10,0 f* loop
    else
      ln10overln2 f* pow2
    then
  else
    ln10overln2 f* pow2
  then
  2-foldable
;

\ -------------------------------------------------------------------------

: s>f ( n -- f ) 0 swap  1-foldable ; \ Signed integer --> Fixpoint s31.32
: f>s ( f -- n ) nip     2-foldable ; \ Fixpoint s31.32 --> Signed integer

\ -----------------------------------------------------------------------------
\   A few tools for dictionary wizardy
\ -----------------------------------------------------------------------------

: executablelocation? ( addr -- ? )
  dup  addrinimem?              \ In imem
  over ramvar-here u< and     \ and below the variables and buffers
  swap addrinemem? or           \ or in emem ?
;

: link>flags ( addr -- addr* ) 1 cells + ;
: link>name  ( addr -- addr* ) 2 cells + ;
: link>code  ( addr -- addr* ) 2 cells + skipstring ;

0 variable searching-for
0 variable closest-found

: code>link  ( entrypoint -- addr | 0 ) \ Try to find this code start address in dictionary

    searching-for !
  0 closest-found !

  dictionarystart
  begin
    dup link>code searching-for @ = if dup closest-found ! then
    dictionarynext
  until
  drop

  closest-found @
;

: inside-code>link ( addr-inside -- addr | 0 ) \ Try to find this address inside of a definition

  dup executablelocation? not if drop 0 exit then  \ Do not try to find locations which are not executable

    searching-for !
  0 closest-found !

  dictionarystart
  begin

    dup link>code searching-for @ u<=
    if \ Is the address of this entry BEFORE the address which is to be found ?
      \ Distance to current   Latest best distance
      searching-for @ over -  searching-for @ closest-found @ -  <
      if dup closest-found ! then \ Is the current entry closer to the address which is to be found ?
    then

    dictionarynext
  until
  drop

  \ Do not cross RAM/IMEM borders:

  searching-for @ addrinimem?
  closest-found @ addrinimem? xor if 0 else closest-found @ then
;

: traceinside. ( addr -- )
  inside-code>link if
  ." ( "                 closest-found @ link>code   hex.
  ." + " searching-for @ closest-found @ link>code - hex.
  ." ) "
  closest-found @ link>name ctype
  then
;

: variable>link  ( location -- addr | 0 ) \ Try to find this variable or buffer in dictionary

    searching-for !
  0 closest-found !

  dictionarystart
  begin

    dup link>flags @ \ Fetch Flags of current definition

    $7FFFFFF0 and \ Snip off visibility bit and alloted size field
    dup $140 = swap $80 = or \ "Buffer" or "Ramallot"
    if
      dup link>code execute searching-for @ = if dup closest-found ! then
    then

    dictionarynext
  until
  drop

  closest-found @
;

: variable-name. ( addr -- ) \ Print the name of this variable or buffer, if possible
  variable>link ?dup if link>name ctype then
;

: forget ( -- ) \ Usage: forget name
  ' code>link
  dup @ (latest) !
  (dp) !
;

: del ( -- ) \ Remove the latest definition in RAM.
    (latest) @ (dp) !
    (latest) @ @ (latest) !
;

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

\ A convenient memory dump helper

: u.4 ( u -- ) 0 <# # # # # #> type ;
: u.2 ( u -- ) 0 <# # # #> type ;

: dump16 ( addr -- ) \ Print 16 bytes memory
  base @ >r hex
  $F bic
  dup hex. ." :  "

  dup 16 + over do
    i c@ u.2 space \ Print data with 2 digits
    i $F and 7 = if 2 spaces then
  loop

  ."  | "

  dup 16 + swap do
        i c@ 32 u>= i c@ 127 u< and if i c@ emit else [char] . emit then
        i $F and 7 = if 2 spaces then
      loop

  ."  |" cr
  r> base !
;

: dump ( addr len -- ) \ Print a memory region
  cr
  over 15 and if 16 + then \ One more line if not aligned on 16
  begin
    swap ( len addr )
    dup dump16
    16 + ( len addr+16 )
    swap 16 - ( addr+16 len-16 )
    dup 0<
  until
  2drop
;

\ Register a C function so it can be called from Forth.
\ Define: ( fun "name" -- )
\ Execute: ( i*x -- j*x )
\ Table contains:
\ - C function pointer.
: c-fun create , does> @ call-c ;

)included_tools";
