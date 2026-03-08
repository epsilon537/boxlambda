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


