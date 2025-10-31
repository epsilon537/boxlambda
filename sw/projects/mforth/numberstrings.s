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

# Zahlenzauber enthält die Routinen für die Umwandlung von Zahlen in Strings und umgekehrt.
# Die Zahlenbasis kann maximal bis 36 gehen, danach fehlt einfach der Zeichensatz.
# Input and Output of numbers. Maximum base is 36.


# -----------------------------------------------------------------------------
# Zahleneingabe - Number input
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
  Definition Flag_visible, "digit" # ( Zeichen -- false / Ziffer true )
digit:  # ( c -- false / u true ) Converts a character into a digit.
# -----------------------------------------------------------------------------

  addi x8, x8, -48      # "0" abziehen.  Subtract "0"
  bltu x8, zero, 5f     # negativ --> Zeichen war "unter Null"  Negative ? --> Invalid character.

  li x15, 10             # Im Bereich bis "9" ?  In range up to "9" ?
  bltu x8, x15, 4f       # Ziffer korrekt erkannt.  Digit recognized properly.

  # Nein: Also ist die Ziffer nicht in den Zahlen 0-9 enthalten gewesen.
  # Prüfe Buchstaben.
  # Character is a letter.

  addi x8, x8, -7  # Anfang der Großbuchstaben, "A"   Beginning of capital letters "A"
# li x15, 10        # Buchstabenwerte beginnen bei 10  Values of letters start with 10
  bltu x8, x15, 5f  # --> Zeichen war ein Sonderzeichen zwischen Ziffern und Großbuchstaben.
                   # --> Character has been a special one between numbers and capital letters.

  li x15, 36        # Es gibt 26 Buchstaben.  26 letters available.
  bltu x8, x15, 4f  # In dem Bereich: Ziffer korrekt erkannt.  In this range ? Digit recognized properly.

  # Für den Fall, dass die Ziffer immer noch nicht erkannt ist, probiere es mit den Kleinbuchstaben.
  # Try to recognize small letters.

  addi x8, x8, -32 # Schiebe zum Anfang der Kleinbuchstaben, "a"  Beginning of small letters "a"
  li x15, 10        # Buchstabenwerte beginnen bei 10  Values of letters start with 10
  bltu x8, x15, 5f  # --> Zeichen war ein Sonderzeichen zwischen Großbuchstaben und Kleinbuchstaben.
                   # --> Character has been a special one between small and capital letters.

  li x15, 36        # Es gibt 26 Buchstaben.  26 letters available.
  bltu x8, x15, 4f  # In dem Bereich: Ziffer korrekt erkannt.  In this range ? Digit recognized properly.

  # Immer noch nicht ? Dann ist das ein Sonderzeichen oberhalb der Kleinbuchstaben oder ein Unicode-Zeichen.
  # Keine gültige Ziffer.
  # Not yet recognized ? --> Character has been a special one above small letters or in Unicode.
  # No valid digit then..


5: # Aussprung mit Fehler  Error.
  li x8, 0    # False-Flag
  ret

4: # Korrekt erkannt. Ziffer in tos

  # Prüfe nun noch, ob die Ziffer innerhalb der Basis liegt !
  # Do not accept digits greater than current base

  laf x15, base
  lc x15, 0(x15)

  bgeu x8, x15, 5b # Außerhalb der Basis werden keine Buchstaben als Zahlen akzeptiert.

  pushdaconst -1  # True-Flag bereitlegen
  ret


#------------------------------------------------------------------------------
  Definition Flag_visible, "number" # Number-Input.
  # ( String Length -- 0 )    Not recognized
  # ( String Length -- n 1 )  Single number
  # ( String Length -- d 2 )  Double number or fixpoint s15.16

number: # Tries to convert a string in one of the supported number formats.
#------------------------------------------------------------------------------

  push_x1_x5_x6_x10_x13

  laf x15, base
  lc x15, 0(x15)
  push x15

/*
    ; Sind noch Zeichen da ? Sonst fertig.
    ; Hole ein Zeichen von vorne.
    ; Prüfe, ob Digit das mag.
    ; Wenn ja: Zahl := Zahl * Basis + neue Ziffer. Zeichen abtrennen.
    ; Wenn nein: Fertig.
    ; Wiederholen.
*/

  # x10: Pointer
  # x11: Characters left
  # x12: Character or helper
  # x13: Sign
  # x5: Result-Low
  # x6: Result-High

  popda x11         # Hole die Länge des Strings  Fetch length of string
  mv x10, x8        # Hole die Stringadresse      Fetch string address
  li x8,  1         # Single length result
  li x13, 1         # Positive Sign
  li x5, 0          # Clear result low
  li x6, 0          #              high

  # ( Single )

  # Check for 'm' style character constants

  li x15, 3
  bne x11, x15, 1f
  li x15, 39 # ASCII '
  lbu x14, 0(x10)
  bne x14, x15, 1f
  lbu x14, 2(x10)
  bne x14, x15, 1f
    lbu x5, 1(x10)
    j 4f

1: # Sind noch Zeichen da ?  Any characters left ?

  beq x11, zero, 4f # String ist leer, bin fertig !

  # Hole ein Zeichen:  Fetch a character
  lbu x12, 0(x10)
  addi x10, x10, 1
  addi x11, x11, -1

  # Vorzeichen und Basisvorsilben:  Sign and base prefixes:
  li x15, 45 # Minus ?
  bne x12, x15, 2f
    li x13, -1
    j 1b

2:li x15, 35 # # ?
  bne x12, x15, 2f
    li x15, 10 # Umschalten auf Dezimal
    laf x12, base
    sc x15, 0(x12)
    j 1b

2:li x15, 36   # $ ?
  bne x12, x15, 2f
    li x15, 16 # Umschalten auf Hexadezimal
    laf x12, base
    sc x15, 0(x12)
    j 1b

2:li x15, 37   # % ?
  bne x12, x15, 2f
    li x15, 2 # Umschalten auf Binär
    laf x12, base
    sc x15, 0(x12)
    j 1b

2:li x15, 46   # . ?
  bne x12, x15, 2f
    li x8, 2 # Double length result !
    j 1b

2:li x15, 44   # , ?
  beq x12, x15, number_nachkommastellen

  # Wandele das Zeichen  Convert character
  pushda x12
  call digit

  popda x15 # Bei false mochte digit das Zeichen nicht.  Error ?
  beq x15, zero, 5f # Aussprung mit Fehler.

  # Zeichen wurde gemocht.  Character has been successfully converted to a digit.


  # Multiply old result with base:
  pushdadouble x5, x6
  #pushda x5 # Low
  #pushda x6 # High

  pushdaaddrf base  # Base-Low
  lc x8, 0(x8)

  pushdaconst 0 # Base-High

  call ud_star

  call rot
  pushdaconst 0 # Make new digit double
  call dplus

  popdadouble x6, x5
  #popda x6 # High
  #popda x5 # Low

  j 1b


4:# String ist leer und wurde korrekt umgewandelt.  String is empty. Almost done...
  # Vorzeichen beachten:  Take care of sign.

  blt zero, x13, 3f

    pushdadouble x5, x6
    #pushda x5
    #pushda x6
    call dnegate
    popdadouble x6, x5
    #popda x6
    #popda x5

3:addi x9, x9, -CELL
  sc x5, 0(x9)  # Low or single result

  li x15, 1  # Check length of result
  beq x8, x15, 3f
    addi x9, x9, -CELL
    sc x6, 0(x9) # High result

3:j 6f

5: # Digit mochte das Zeichen nicht. Return without success.
  li x8, 0  # No result

6: # Finished. Restore base and return.
  pop x15
  laf x14, base
  sc x15, 0(x14)

  pop_x1_x5_x6_x10_x13
  ret


number_nachkommastellen: # Digits after the decimal point.
  mv x6, x5   # Low part is "high" part before comma now.
  li x5, 0     # Clear low part. To be filled with digits after comma.
  li x8, 2      # Double length result !

1: # Sind noch Zeichen da ?  Any characters left ?
  beq x11, zero, 4b # String ist leer, bin fertig !

  # Fetch a character from end of string:
  addi x11, x11, -1 # Länge um eins verringern
  add x15, x10, x11
  lbu x12, 0(x15) # Zeichen holen.

  li x15, 46         # . ?
  beq x12, x15, 1b   # Accept more dots for clarity, already double result.

  # Wandele das Zeichen  Convert character
  pushda x12
  call digit

  popda x15 # Bei false mochte digit das Zeichen nicht.  Error ?
  beq x15, zero, 5b # Aussprung mit Fehler.

  # Zeichen wurde gemocht.  Character has been successfully converted to a digit.

  addi x9, x9, -CELL
  sc x5, 0(x9) # Low-Old
  pushdaaddrf base
  lc x8, 0(x8)

  # ( Old New-Digit Base )
  call um_slash_mod # ( Remainder New.. )

  mv x5, x8
  ddrop
  # popda x5
  # drop

  j 1b
