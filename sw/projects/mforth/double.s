#
#    Mecrisp-Quintus - A native code Forth implementation for RISC-V
#    Copyright (C) 2018  Matthias Koch
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Double number support

#------------------------------------------------------------------------------
# --- Double stack jugglers ---
#------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "2dup" # ( 2 1 -- 2 1 2 1 )
# -----------------------------------------------------------------------------
  ddup
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "2drop" # ( 2 1 -- )
ddrop_vektor:
# -----------------------------------------------------------------------------
  ddrop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_4, "2swap" # ( 4 3 2 1 -- 2 1 4 3 )
dswap:
# -----------------------------------------------------------------------------
  mv x15, x8
  lc  x8, 1*CELL(x9)
  sc x15, 1*CELL(x9)

  lc x15, 0*CELL(x9)
  lc x14, 2*CELL(x9)
  sc x15, 2*CELL(x9)
  sc x14, 0*CELL(x9)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "2nip" # ( 4 3 2 1 -- 2 1 )
dnip:
# -----------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, 2*CELL
  sc x15, 0(x9)
  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_4, "2over" # ( 4 3 2 1 -- 4 3 2 1 4 3 )
# -----------------------------------------------------------------------------

  #    8  4  0    t
  # (  4  3  2    1 )
  #   16 12  8    4  0  t
  # (  4  3  2    1  4  3 )

  addi x9, x9, -2*CELL
  sc x8,  1*CELL(x9)

  lc x15, 4*CELL(x9)
  sc x15, 0*CELL(x9)

  lc x8,  3*CELL(x9)

  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_4, "2tuck" # ( 4 3 2 1 -- 2 1 4 3 2 1 )
# -----------------------------------------------------------------------------

  #    8  4  0  t
  # (  4  3  2  1 )
  #   16 12  8  4  0  t
  # (  2  1  4  3  2  1 )

  #  x8               # 1
  lc x14, 0*CELL(x9)  # 2
  #       1*CELL(x9)  # 3
  lc x15, 2*CELL(x9)  # 4

  addi x9, x9, -2*CELL # Zwei Elemente mehr

  sc x14, 0*CELL(x9)
  sc x14, 4*CELL(x9)

  lc x14, 3*CELL(x9)  # Alte Position 4
  sc x14, 1*CELL(x9)
  sc x15, 2*CELL(x9)

  sc  x8, 3*CELL(x9)  # Alte Position 4

  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_6, "2rot" # ( 6  5 4 3 2 1  -- 4  3 2 1 6 5 ) ( x w y -- w y x )
                                     #  16 12 8 4 0 tos  16 12 8 4 0 tos
# -----------------------------------------------------------------------------

  lc x15, 0*CELL(x9)
  lc x14, 2*CELL(x9)
  sc x15, 2*CELL(x9)

  lc x15, 4*CELL(x9)
  sc x14, 4*CELL(x9)
  sc x15, 0*CELL(x9)

  lc x14, 1*CELL(x9)
  sc x8,  1*CELL(x9)
  lc x8,  3*CELL(x9)
  sc x14, 3*CELL(x9)

  ret

# -----------------------------------------------------------------------------
  Definition Flag_foldable_6, "2-rot" # ( 6  5 4 3 2 1 --  2  1 6 5 4 3 ( x w y -- y x w )
                                      #  16 12 8 4 0 tos  16 12 8 4 0 tos
# -----------------------------------------------------------------------------

  lc x14, 0*CELL(x9)
  lc x15, 4*CELL(x9)
  sc x14, 4*CELL(x9)

  lc x14, 2*CELL(x9)
  sc x14, 0*CELL(x9)
  sc x15, 2*CELL(x9)

  lc x14, 3*CELL(x9)
  sc x8,  3*CELL(x9)
  lc x8,  1*CELL(x9)
  sc x14, 1*CELL(x9)

  ret

#------------------------------------------------------------------------------
# --- Double return stack jugglers ---
#------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
  Definition Flag_inline, "2>r" # ( 2 1 -- R: -- 2 1 )
# -----------------------------------------------------------------------------
  to_r_2
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline, "2r>" # ( -- 2 1 R: 2 1 -- )
# -----------------------------------------------------------------------------
  r_from_2
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline, "2r@" # ( -- 2 1 R: 2 1 -- 2 1 )
# -----------------------------------------------------------------------------
  r_fetch_2
  ret

# -----------------------------------------------------------------------------
  Definition Flag_inline, "2rdrop" # ( -- R: 2 1 -- )
# -----------------------------------------------------------------------------
  addi sp, sp, 2*CELL
  ret

#------------------------------------------------------------------------------
# --- Double calculations ---
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "d2/"
#------------------------------------------------------------------------------
  lc x15, 0(x9) # Low

  slli x14, x8, SIGNSHIFT
  srai x8, x8, 1

  srli x15, x15, 1
  or x15, x15, x14

  sc x15, 0(x9)
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "d2*"
#------------------------------------------------------------------------------
  lc x15, 0(x9) # Low

  srli x14, x15, SIGNSHIFT
  add x15, x15, x15

  add x8, x8, x8
  or x8, x8, x14

  sc x15, 0(x9)
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "dshr"
#------------------------------------------------------------------------------
  lc x15, 0(x9) # Low

  slli x14, x8, SIGNSHIFT
  srli x8, x8, 1

  srli x15, x15, 1
  or x15, x15, x14

  sc x15, 0(x9)
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "dshl"
#------------------------------------------------------------------------------
  lc x15, 0(x9) # Low

  srli x14, x15, SIGNSHIFT
  add x15, x15, x15

  add x8, x8, x8
  or x8, x8, x14

  sc x15, 0(x9)
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "dabs"
#------------------------------------------------------------------------------
dabs:
  blt x8, zero, dnegate # Check sign in high-part
  ret                   # Not negative ? Nothing to do !

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "dnegate"
dnegate:
#------------------------------------------------------------------------------
  lc x15, 0(x9) # Low
  inv x15
  inv x8
  addi x14, x15, 1
  sc x14, 0(x9)
  sltu x15, x14, x15
  add x8, x8, x15
  ret

.macro addc dest, sour1, sour2
  add x5, \sour1, \sour2
  sltu x6, x5, \sour1
  add \dest, x5, x15
  sltu x7, \dest, x5
  or x15, x6, x7
.endm

.macro subc dest, sour1, sour2
  sub x5, \sour1, \sour2
  sltu x6, \sour1, \sour2
  sub \dest, x5, x15
  sltu x7, x5, x15
  or x15, x6, x7
.endm

#------------------------------------------------------------------------------
  Definition Flag_foldable_4, "d-" # ( 1L 1H 2L 2H )
dminus:                            #   8  4  0  x8
#------------------------------------------------------------------------------
  push x10

  lc x15, 2*CELL(x9)
  lc x14, 0*CELL(x9)

  sub x10, x15, x14
  sc x10, 2*CELL(x9)

  sltu x10, x15, x14

  lc x15, 1*CELL(x9)
  sub x8, x15, x8
  sub x8, x8, x10

  addi x9, x9, 2*CELL

  pop x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_4, "d+" # ( 1L 1H 2L 2H )
dplus:                             #   8  4  0  x8
#------------------------------------------------------------------------------
  push x10

  lc x15, 2*CELL(x9)
  lc x14, 0*CELL(x9)

  add x10, x15, x14
  sc x10, 2*CELL(x9)

  sltu x10, x10, x15

  lc x15, 1*CELL(x9)
  add x8, x15, x8
  add x8, x8, x10

  addi x9, x9, 2*CELL

  pop x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_1, "s>d" # ( n - dl dh ) Single --> Double conversion
#------------------------------------------------------------------------------
  pushdatos
  srai x8, x8, SIGNSHIFT
  ret

#------------------------------------------------------------------------------
# --- Double star and slash ---
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "ud*"
ud_star:
         # Unsigned multiply 64*64 = 64
         # ( ud1 ud2 -- ud )
#------------------------------------------------------------------------------
  push x1
  call udm_star
  ddrop
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "udm*"
udm_star: # Unsigned multiply 64*64 = 128
          # ( ud1 ud2 -- udl udh )
#------------------------------------------------------------------------------
  # Auf dem Datenstack: ( 1L 1H 2L 2H -- LL  L  H HH )
  #                       12  8  4  0 nach pushdatos
  #                        d  c  b  a    r0 r1 r2 r3
  # Benötige einen langen Ergebnisregister !

  push_x1_x5_x6_x7_x10_x13

  # ( d c b a )
  pushdatos
  lc x8, 1*CELL(x9)   # b
  pushdatos
  lc x8, 4*CELL(x9)   # d
  call um_star
  # ( d c b a  b*d-Low b*d-High )
  popdadouble x11, x10
  # popda x11 # b*d-High
  # popda x10 # b*d-Low, finished value

  # ( d c b a )

  pushdatos
  lc x8, 0*CELL(x9)  # a
  pushdatos
  lc x8, 3*CELL(x9)  # c
  call um_star
  # ( d c b a  a*c-Low a*c-High )
  popdadouble x13, x12
  # popda x13 # a*c-High
  # popda x12 # a*c-Low

  # ( d c b a )

  pushdatos
  lc x8, 0*CELL(x9)   # a
  pushdatos
  lc x8, 4*CELL(x9)   # d

  call um_star
  # ( d c b a  a*d-Low a*d-High )

  li x15, 0
  addc x12, x12, x8    # a*c-Low + a*d-High
  addc x13, x13, zero  # Carry
  drop

  li x15, 0
  addc x11, x11, x8 # a*d-Low + b*d-High
  addc x12, x12, zero # Carry
  addc x13, x13, zero # Carry
  drop

  # ( d c b a )

  pushdatos
  lc x8, 1*CELL(x9)   # b
  pushdatos
  lc x8, 3*CELL(x9)   # c

  call um_star
  # ( d c b a  b*c-Low b*c-High )

  li x15, 0
  addc x12, x12, x8 # a*c-Low + b*c-High + a*d-High
  addc x13, x13, zero # Carry
  drop

  li x15, 0
  addc x11, x11, x8 # b*c-Low + a*d-Low + b*d-High
  addc x12, x12, zero  # Carry
  addc x13, x13, zero  # Carry
  drop

  # ( d c b tos: a )
  mv x8, x13
  sc x12, 0*CELL(x9)
  sc x11, 1*CELL(x9)
  sc x10, 2*CELL(x9)

  pop_x1_x5_x6_x7_x10_x13
  ret


#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_3, "*/" # Signed scalar
  # ( u1 u2 u3 -- u1*u2/u3 ) With double length intermediate result
#------------------------------------------------------------------------------
  push x1
  to_r
  call m_star
  r_from
  call m_slash_mod
  nip
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_3, "*/mod" # Signed scalar
  # ( u1 u2 u3 -- u1*u2/u3 ) With double length intermediate result
#------------------------------------------------------------------------------
  push x1
  to_r
  call m_star
  r_from
  pop x1
  j m_slash_mod

# : u*/  ( u1 u2 u3 -- u1 * u2 / u3 )  >r um* r> um/mod nip 3-foldable ;
#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_3, "u*/" # Unsigned scalar
  # ( u1 u2 u3 -- u1*u2/u3 ) With double length intermediate result
#------------------------------------------------------------------------------
  push x1
  to_r
  call um_star
  r_from
  call um_slash_mod
  nip
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_3, "u*/mod" # Unsigned scalar
  # ( u1 u2 u3 -- u1*u2/u3 ) With double length intermediate result
#------------------------------------------------------------------------------
  push x1
  to_r
  call um_star
  r_from
  pop x1
  j um_slash_mod

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_3, "um/mod"
um_slash_mod: # ( ud u -- u u ) Dividend Divisor -- Rest Ergebnis
             # 64/32 = 32 Rest 32
#------------------------------------------------------------------------------
  push x1
  pushdaconst 0
  call ud_slash_mod
  drop
  nip
  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_3, "m/mod"
              # Signed symmetric divide 64/32 = 32 remainder 32
m_slash_mod:  # ( d n -- n n )
#------------------------------------------------------------------------------
  push x1
  pushdatos               # s>d
  srai x8, x8, SIGNSHIFT
  call d_slash_mod
  drop
  nip
  pop x1
  ret








#------------------------------------------------------------------------------
#  Definition Flag_visible|Flag_foldable_4, "uf/mod" # Internal helper only.
uf_slash_mod: # Divide 64/64 = 64 Remainder 64. Puts decimal point in the middle. Overflow possible.
         # ( ud1 ud2 -- ud ud)
         # ( 1L 1H 2L tos: 2H -- Rem-L Rem-H Quot-L tos: Quot-H )
#------------------------------------------------------------------------------

   push_x10_x13

   li x13, 0
   lc x12, 1*CELL(x9)
   lc x11, 2*CELL(x9)
   li x10, 0

   j ud_slash_mod_internal


#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "ud/mod"
         # Unsigned divide 64/64 = 64 remainder 64
         # ( ud1 ud2 -- ud ud)
         # ( 1L 1H 2L tos: 2H -- Rem-L Rem-H Quot-L tos: Quot-H )
#------------------------------------------------------------------------------
ud_slash_mod:


   # ( DividendL DividendH DivisorL DivisorH -- RemainderL RemainderH ResultL ResultH )
   #   8         4         0        tos      -- 8          4          0       tos


   # Shift-High Shift-Low Dividend-High Dividend-Low
   #        x13       x12           x11          x10

   push_x10_x13

   li x13, 0
   li x12, 0
   lc x11, 1*CELL(x9)
   lc x10, 2*CELL(x9)

   # Divisor-High Divisor-Low
   #         x4          x3

ud_slash_mod_internal:

   push_x3_x4_x5_x6_x7
    # We need a lot of registers here.
    # Repurpose loop registers !

   mv x4, x8
   lc x3, 0(x9)

   # For this long division, we need 64 individual division steps.
   li x8, 2*CELLBITS

3:
    # Shift the long chain of four registers.

    slli x13, x13, 1
      srli x15, x12, SIGNSHIFT
      or x13, x13, x15

    slli x12, x12, 1
      srli x15, x11, SIGNSHIFT
      or x12, x12, x15

    slli x11, x11, 1
      srli x15, x10, SIGNSHIFT
      or x11, x11, x15

    slli x10, x10, 1

    # Compare Divisor with top two registers
    bltu x4, x13, 1f # Check high part first
    bltu x13, x4, 2f

    bltu x12, x3, 2f # High part is identical. Low part decides.

    # Subtract Divisor from two top registers
1:  li x15, 0
    subc x12, x12, x3
    subc x13, x13, x4

    # Insert a bit into Result which is inside LSB of the long register.
    addi x10, x10, 1
2:

   addi x8, x8, -1
   bne x8, zero, 3b

   # Now place all values to their destination.
   mv x8, x11    # Result-High
   sc x10, 0*CELL(x9) # Result-Low
   sc x13, 1*CELL(x9) # Remainder-High
   sc x12, 2*CELL(x9) # Remainder-Low

   pop_x3_x4_x5_x6_x7

   pop_x10_x13
   ret


#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "d/mod"
              # Signed symmetric divide 64/64 = 64 remainder 64
              # ( d1 d2 -- d d )
d_slash_mod:  # ( 1L 1H 2L tos: 2H -- Rem-L Rem-H Quot-L tos: Quot-H )
#------------------------------------------------------------------------------
  # Check Divisor
  push x1

  # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
  # beq 2f
  bge x8, zero, 2f

    # ? / -
    call dnegate
    call dswap
    # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
    # beq 1f
    bge x8, zero, 1f

    # - / -
    call dnegate
    call dswap
    call ud_slash_mod

    call dswap
    call dnegate # Negative remainder
    pop x1
    j dswap


1:  # + / -
    call dswap
    call ud_slash_mod
    pop x1
    j dnegate  # Negative result


2:  # ? / +
    call dswap
    # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
    # beq 3f
    bge x8, zero, 3f

    # - / +
    call dnegate
    call dswap

    call ud_slash_mod

    call dnegate # Negative result
    call dswap
    call dnegate # Negative remainder
    pop x1
    j dswap


3:  # + / +
    call dswap
    pop x1
    j ud_slash_mod

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "d/"
#------------------------------------------------------------------------------
  push x1
  call d_slash_mod
  pop x1
  j dnip

#------------------------------------------------------------------------------
# --- s31.32 calculations ---
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "f*"
f_star: # Signed multiply s31.32
        # ( fi fi -- fi )
        # Overflow possible. Sign wrong in this case.
#------------------------------------------------------------------------------
  push x1
  # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
  # beq 1f
  bge x8, zero, 1f

  # - * ?
    call dnegate
    call dswap
    # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
    # beq 2f # - * +
    bge x8, zero, 2f

    # - * -
    call dnegate

3:  # + * +, - * -
    call udm_star
    # ( LL L H HH )
    drop
    # ( LL L H )
    lc x15, 0(x9)
    addi x9, x9, CELL
    sc x15, 0(x9)
    # ( L H )
    pop x1
    ret

1:# + * ?
    call dswap
    # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
    # beq 3b # + * +
    bge x8, zero, 3b

    call dnegate

    # - * + or + * -
2:  call udm_star
    # ( LL L H HH )
    drop
    # ( LL L H )
    lc x15, 0(x9)
    addi x9, x9, CELL
    sc x15, 0(x9)
    # ( L H )
  pop x1
    j dnegate

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_foldable_4, "f/"
  # Signed divide for s31.32. Overflow possible. Sign wrong in this case.
#------------------------------------------------------------------------------
  # Take care of sign ! ( 1L 1H 2L 2H - EL EH )
  push x1

  # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
  # beq 2f
  bge x8, zero, 2f

  # ? / -
    call dnegate
    call dswap
    # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
    # beq 3f # + / -
    bge x8, zero, 3f

    # - / -
    call dnegate
1:  call dswap # - / - or + / +
    call uf_slash_mod
    call dnip
    pop x1
    ret

2:# ? / +
  call dswap
  # movs r0, tos, asr #31 # Turn MSB into 0xffffffff or 0x00000000
  # beq 1b # + / +
  bge x8, zero, 1b

  # - / +
  call dnegate
3:call dswap # - / + or + / -
  call uf_slash_mod
  call dnegate
  pop x1
  j dnip

#------------------------------------------------------------------------------
# --- Double memory ---
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
  Definition Flag_visible, "2!" # Store ( d addr -- )
#------------------------------------------------------------------------------

  lc x15, 0*CELL(x9)
  lc x14, 1*CELL(x9)

  sc x15, 0*CELL(x8)
  sc x14, 1*CELL(x8)

  lc x8,  2*CELL(x9)
  addi x9, x9, 3*CELL

  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "2@" # Fetch ( addr -- d )
#------------------------------------------------------------------------------
  addi x9, x9, -CELL
  lc x15, 1*CELL(x8)
  sc x15, 0*CELL(x9)
  lc x8,  0*CELL(x8)
  ret

#------------------------------------------------------------------------------
# --- Double comparisions ---
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
Definition Flag_foldable_4, "du<"
#------------------------------------------------------------------------------
du_less:
  push x10

  lc x15, 2*CELL(x9)
  lc x14, 0*CELL(x9)

  sltu x10, x15, x14

  lc x15, 1*CELL(x9)

  sltu x14, x15, x10
  sub  x15, x15, x10
  sltu x15, x15, x8

  or x8, x15, x14

  addi x8, x8, -1
  inv x8

  addi x9, x9, 3*CELL
  pop x10
  ret


#------------------------------------------------------------------------------
Definition Flag_foldable_4, "du>"  # Just swapped the order of registers
#------------------------------------------------------------------------------
  push x1
  call dswap
  pop x1
  j du_less

#  From Mecrisp-Ice:
#  wire [16:0] minus = {1'b1, ~st0} + st1 + 1;
#  wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];

#------------------------------------------------------------------------------
Definition Flag_foldable_4, "d<"
#------------------------------------------------------------------------------
d_less:
  push_x1_x10_x11

  srai x10, x8, SIGNSHIFT
  lc x11, 1*CELL(x9)
  srai x11, x11, SIGNSHIFT

  call du_less

  xor x8, x8, x10
  xor x8, x8, x11

  pop_x1_x10_x11
  ret


#------------------------------------------------------------------------------
Definition Flag_foldable_4, "d>"  # Just swapped the order of registers
#------------------------------------------------------------------------------
  push x1
  call dswap
  pop x1
  j d_less

#------------------------------------------------------------------------------
  Definition Flag_foldable_2|Flag_inline|Flag_noframe, "d0<" # ( 1L 1H -- Flag ) Is double number negative ?
#------------------------------------------------------------------------------
  addi x9, x9, CELL
  srai x8, x8, SIGNSHIFT
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_2, "d0=" # ( 1L 1H -- Flag )
#------------------------------------------------------------------------------
  lc x15, 0(x9)
  addi x9, x9, CELL
  or x8, x8, x15

  sltiu x8, x8, 1
  sub x8, zero, x8

  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_4, "d<>" # ( 1L 1H 2L 2H -- Flag )
#------------------------------------------------------------------------------
  lc x15, 1*CELL(x9)
  xor x8, x8, x15

  lc x15, 0*CELL(x9)
  lc x14, 2*CELL(x9)
  xor x15, x15, x14
  addi x9, x9, 3*CELL

  or x8, x8, x15

  sltiu x8, x8, 1
  addi x8, x8, -1

  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_4, "d=" # ( 1L 1H 2L 2H -- Flag )
#------------------------------------------------------------------------------
  lc x15, 1*CELL(x9)
  xor x8, x8, x15

  lc x15, 0*CELL(x9)
  lc x14, 2*CELL(x9)
  xor x15, x15, x14
  addi x9, x9, 3*CELL

  or x8, x8, x15

  sltiu x8, x8, 1
  sub x8, zero, x8

  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_3, "2lshift"
#------------------------------------------------------------------------------

  andi x8, x8, 2*CELLBITS-1 # Auch Doppeltschübe seien zyklisch
  beq x8, zero, dshift_drop

  addi x14, x8, -CELLBITS # Fallunterscheidung, ob 32 oder mehr Stellen geschoben werden sollen
  blt x14, zero, dlshift_short

  # 32 oder mehr Stellen weit schieben:

  lc x15, 1*CELL(x9)
  sll x8, x15, x14
  addi x9, x9, CELL
  sc zero, 0(x9)
  ret

dlshift_short:  # 31 oder weniger Stellen weit schieben

  lc  x15, 1*CELL(x9) # Low-Teil zuerst
  sll x14, x15, x8    # Low << n
  sc  x14, 1*CELL(x9)

  # Low in x15 enthalten, jetzt den High-Teil in Angriff nehmen:

  li  x14, CELLBITS
  sub x14, x14, x8
  srl x15, x15, x14    # Low >> 32-n

  lc  x14, 0(x9)
  sll x8, x14, x8
  or  x8, x8, x15

  addi x9, x9, CELL
  ret


#------------------------------------------------------------------------------
  Definition Flag_foldable_3, "2rshift"
#------------------------------------------------------------------------------

  andi x8, x8, 2*CELLBITS-1 # Auch Doppeltschübe seien zyklisch
  beq x8, zero, dshift_drop

  addi x14, x8, -CELLBITS # Fallunterscheidung, ob 32 oder mehr Stellen geschoben werden sollen
  blt x14, zero, drshift_short

  # 32 oder mehr Stellen weit schieben:

  lc x15, 0(x9)  # High-Teil laden
  srl x15, x15, x14
  addi x9, x9, CELL
  sc x15, 0(x9)  # Als Low-Teil speichern
  li x8, 0
  ret

drshift_short:  # 31 oder weniger Stellen weit schieben

  lc  x15, 0(x9)    # High-Teil zuerst
  srl x14, x15, x8  # High >> n
  sc  x14, 0(x9)

  # High in x15 enthalten, jetzt den Low-Teil in Angriff nehmen:

  li  x14, CELLBITS
  sub x14, x14, x8
  sll x15, x15, x14    # High << 32-n

  lc  x14, 1*CELL(x9)
  srl x14, x14, x8
  or  x14, x14, x15
  sc  x14, 1*CELL(x9)

  drop
  ret

#------------------------------------------------------------------------------
  Definition Flag_foldable_3, "2arshift"
#------------------------------------------------------------------------------

  andi x8, x8, 2*CELLBITS-1 # Auch Doppeltschübe seien zyklisch
  beq x8, zero, dshift_drop

  addi x14, x8, -CELLBITS # Fallunterscheidung, ob 32 oder mehr Stellen geschoben werden sollen
  blt x14, zero, darshift_short

  # 32 oder mehr Stellen weit schieben:

  lc x15, 0(x9)  # High-Teil laden
  sra x8, x15, SIGNSHIFT
  sra x15, x15, x14
  addi x9, x9, CELL
  sc x15, 0(x9)  # Als Low-Teil speichern

  ret

darshift_short:  # 31 oder weniger Stellen weit schieben

  lc  x15, 0(x9)    # High-Teil zuerst
  sra x14, x15, x8  # High >> n
  sc  x14, 0(x9)

  # High in x15 enthalten, jetzt den Low-Teil in Angriff nehmen:

  li  x14, CELLBITS
  sub x14, x14, x8
  sll x15, x15, x14    # High << 32-n

  lc  x14, 1*CELL(x9)
  srl x14, x14, x8
  or  x14, x14, x15
  sc  x14, 1*CELL(x9)

dshift_drop:
  drop
  ret

