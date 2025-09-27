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

# -----------------------------------------------------------------------------
# Zahlenausgabe - Number output
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
  Definition Flag_visible, ".digit" # ( Ziffer -- Zeichen ) Wandelt eine Ziffer in ein Zeichen um.
               # Wenn ein Zahlensystem größer 36 angestrebt wird,
               # werden nicht druckbare Zeichen einfach mit # beschrieben.
digitausgeben: # ( u -- c ) Converts a digit into a character.
               # If base is bigger than 36, unprintable digits are written as #
# -----------------------------------------------------------------------------
  li x15, 10
  bgeu x8, x15, 1f # Von 0-9:
    addi x8, x8, 48 # Schiebe zum Anfang der Zahlen  Shift to beginning of ASCII numbers
    ret

1:li x15, 36
  bgeu x8, x15, 2f # Von A-Z:
    addi x8, x8, 55 # Alternative für Kleinbuchstaben: 87.                 For small letters: 87.
    ret

2:li x8, 35 # Zeichen #, falls diese Ziffer nicht darstellbar ist. Character #, if digit is not printable
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "hold" # Fügt dem Zahlenstring von vorne ein Zeichen hinzu.
hold: # ( Zeichen -- )  Insert one character at the beginning of number buffer
#------------------------------------------------------------------------------

  # Alter String:  | Länge     |     |
  # Neuer String:  | Länge + 1 | Neu |

  # Alter String:  | Länge     | I   | II  | III |     |
  # Neuer String:  | Länge + 1 | Neu | I   | II  | III |

  # Old String:  | Length     |     |
  # New String:  | Length + 1 | New |

  # Old String:  | Length     | I   | II  | III |     |
  # New String:  | Length + 1 | New | I   | II  | III |

  push x10

  laf x10, Zahlenpuffer
  lbu x14, 0(x10) # Länge holen

  li x15, Zahlenpufferlaenge  # Ist der Puffer voll ? Number buffer full ?
  bgeu x14, x15, 3f           # Keine weiteren Zeichen mehr annehmen.

  # Länge des Puffers um 1 erhöhen  Increment length
  addi x14, x14, 1
  sb x14, 0(x10) # Aktualisierte Länge schreiben

  # Am Ende anfangen:  Start moving with the end
  add x10, x10, x14 # Zeiger an die freie Stelle für das neue Zeichen

  # Ist die Länge jetzt genau 1 Zeichen ? Dann muss ich nichs schieben.
  li x15, 1 # Check if at least one character has to be moved
  beq x14, x15, 2f

1:# Schiebeschleife:  Move characters !
  addi x10, x10, -1
  lbu x15, 0(x10) # Holen an der Stelle-1  Fetch from current location-1
  sb x15, 1(x10)  # Schreiben an der Stelle  Write current location

  addi x14, x14, -1
  li x15, 1
  bne x14, x15, 1b # Bis nur noch ein Zeichen bleibt. Das ist das Neue.
                  # Until there is only one character left - the new one.

2:# Das neue Zeichen an seinen Platz legen
  sb x8, 0(x10) # Insert new character

3:drop
  pop x10
  ret

#------------------------------------------------------------------------------
 Definition Flag_visible, "hold<"
zahlanhaengen: # ( Character -- ) Insert one character at the end of number buffer
#------------------------------------------------------------------------------
  push x10

  laf x10, Zahlenpuffer
  lbu x14, 0(x10) # Länge holen

  li x15, Zahlenpufferlaenge  # Ist der Puffer voll ? Number buffer full ?
  bgeu x14, x15, 3f           # Keine weiteren Zeichen mehr annehmen.

    addi x14, x14, 1 # Ein Zeichen mehr
    sb x14, 0(x10) # Neue Länge schreiben
    add x10, x10, x14
    sb x8, 0(x10) # Neues Zeichen am Ende anhängen

3:drop
  pop x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "sign"
vorzeichen: # ( Vorzeichen -- )
      # Prüft die Zahl auf dem Stack auf ihr Vorzeichen hin und
      # fügt bei Bedarf ein Minus an den Ziffernstring an.
      # Checks flag of number on stack and adds a minus to number buffer if it is negative.
#------------------------------------------------------------------------------
  blt x8, zero, 1f
  drop
  ret

1:li x8, 45  # Minuszeichen  ASCII for minus
  j hold     # an den Zahlenpuffer anhängen  put it into number buffer

#------------------------------------------------------------------------------
  Definition Flag_visible, "#>" # ( ZahlenrestL (ZahlenrestH) -- Addr Len )
zifferstringende:  # Schließt einen neuen Ziffernstring ab und gibt seine Adresse zurück.
                   # Benutzt dafür einfach den Zahlenpuffer.
                   # Finishes a number string and gives back its address.
#------------------------------------------------------------------------------
  laf x15, Zahlenpuffer
  lbu x8, 0(x15)
  addi x15, x15, 1
  sc x15, 0(x9)
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "f#S"
falleziffern: # ( u -- u=0 )
      # Inserts all digits, at least one, into number buffer.
#------------------------------------------------------------------------------
  push_x1_x10

  li x10, CELLBITS

1:call fziffer
  addi x10, x10, -1
  bne x10, zero, 1b

  pop_x1_x10
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "f#"
fziffer: # ( u -- u )
      # Insert one more digit into number buffer
#------------------------------------------------------------------------------
  # Handles parts after decimal point
  # Idea: Multiply with base, next digit will be shifted into high-part of multiplication result.
  push x1
    pushdaaddrf base
    lc x8, 0(x8)
    call um_star     # ( After-Decimal-Point Base -- Low High )
    call digitausgeben # ( Low=Still-after-decimal-point Character )
  pop x1
    j zahlanhaengen # Add character to number buffer

#------------------------------------------------------------------------------
  Definition Flag_visible, "#S"
alleziffern: # ( d-Zahl -- d-Zahl=0 )
      # Fügt alle Ziffern, jedoch mindestens eine,
      # an den im Aufbau befindlichen String an.
      # Inserts all digits, at least one, into number buffer.
#------------------------------------------------------------------------------
  push x1

1:call ziffer
  bne x8, zero, 1b

  lc x15, 0(x9)
  bne x15, zero, 1b

  pop x1
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "#"
ziffer: # ( Zahl -- Zahl )
      # Fügt eine weitere Ziffer hinzu, Rest entsprechend verkleinert.
      # Insert one more digit into number buffer
#------------------------------------------------------------------------------
  # Idea: Divide by base. Remainder is digit, Result is to be handled in next run.
  # Idee dahinter: Teile durch die Basis.
  # Bekomme einen Rest, und einen Teil, den ich im nächsten Durchlauf
  # behandeln muss. Der Rest ist die Ziffer.
  push x1
    pushdaaddrf base
    lc x8, 0(x8)   # Base-Low
    pushdaconst 0  # Base-High
    # ( uL uH BaseL BaseH )
    call ud_slash_mod
    # ( RemainderL RemainderH uL uH )
    call dswap
    # ( uL uH RemainderL RemainderH )
    drop
    # ( uL uH RemainderL )
    call digitausgeben
    # ( uL uH digit )
  pop x1
    j hold
    # ( uL uH )

#------------------------------------------------------------------------------
  Definition Flag_visible, "<#" # ( d-Zahl -- d-Zahl )
zifferstringanfang: # Eröffnet einen neuen Ziffernstring.
                    # Opens a number string
#------------------------------------------------------------------------------
  laf x14, Zahlenpuffer # Länge löschen, bisherige Länge Null.
  li x15, 0
  sb x15, 0(x14)
  ret

#------------------------------------------------------------------------------
  Definition Flag_visible, "f."
      # ( Low High -- )
      # Prints a s31.32 number
#------------------------------------------------------------------------------
  pushdaconst CELLBITS
  j fdotn

#------------------------------------------------------------------------------
  Definition Flag_visible, "f.n"
      # ( Low High n -- )
      # Prints a s31.32 number with given number of fractional digits
fdotn:
#------------------------------------------------------------------------------
  push_x1_x10
  popda x10

  # ( Low High -- )
  call tuck # ( Sign Low High )
  call dabs # ( Sign uLow uHigh )

  pushdaconst 0  # ( Sign After-decimal-point=uL Before-decimal-point-low=uH Before-decimal-point-high=0 )
  call zifferstringanfang

  call alleziffern # Processing of high-part finished. ( Sign uL 0 0 )
  drop # ( Sign uL 0 )

  li x8, 44 # Add a comma to number buffer ( Sign uL 44 )
  call zahlanhaengen # ( Sign uL )

  beq x10, zero, 2f

1:call fziffer   # Processing of fractional parts ( Sign 0 )
  addi x10, x10, -1
  bne x10, zero, 1b

2:pop x10
  drop
  call vorzeichen

  pushdatos # Will be removed later
  pushdatos
  j abschluss_zahlenausgabe

#------------------------------------------------------------------------------
  Definition Flag_visible, "ud." # ( ud -- )
uddot:  # Prints an unsigned double number
#------------------------------------------------------------------------------
  # In Forth: <# #S #>
  push x1
  call zifferstringanfang
  call alleziffern
  j abschluss_zahlenausgabe

#------------------------------------------------------------------------------
  Definition Flag_visible, "d." # ( d -- )
ddot:   # Prints a signed double number
#------------------------------------------------------------------------------
  push x1
  call tuck
  call dabs

  call zifferstringanfang
  call alleziffern # ( Sign 0 0 )
  call rot
  call vorzeichen

abschluss_zahlenausgabe:
  call zifferstringende
  call type
  pop x1
  j space

# -----------------------------------------------------------------------------
  Definition Flag_visible, "u."
      # ( Zahl -- )
      # Gibt eine vorzeichenlose Zahl aus.
      # Prints an unsigned single number
# -----------------------------------------------------------------------------
udot:
  pushdaconst 0 # Convert to unsigned double
  j uddot

# -----------------------------------------------------------------------------
  Definition Flag_visible, "." # ( Zahl -- )
     # Gibt eine vorzeichenbehaftete Zahl aus.
     # Prints a signed single number
# -----------------------------------------------------------------------------
dot:
  pushdatos
  srai x8, x8, SIGNSHIFT # s>d
  j ddot

