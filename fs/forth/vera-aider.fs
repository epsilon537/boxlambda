( x y val addr width bpp -- )
: (pixel-set)
  \ Would a jump table be better here?
  case ( pxlval base y width x )
    1 of (pixel-set-1bpp) endof
    2 of (pixel-set-2bpp) endof
    4 of (pixel-set-4bpp) endof
    8 of (pixel-set-8bpp) endof
    true assert
  endcase
;

( x y val addr width bpp )
: (pixel-set)
  5 pick swap ( x y val addr width x bpp )
;

( base y width x -- pxlval )
: (pixel-get)
  \ Would a jump table be better here?
  case ( base y width x )
    1 of (pixel-get-1bpp) endof
    2 of (pixel-get-2bpp) endof
    4 of (pixel-get-4bpp) endof
    8 of (pixel-get-8bpp) endof
    true assert
  endcase
;

( pxlval base y width x bpp -- )
: (pixel-set)
  \ Would a jump table be better here?
  case ( pxlval base y width x )
    1 of (pixel-set-1bpp) endof
    2 of (pixel-set-2bpp) endof
    4 of (pixel-set-4bpp) endof
    8 of (pixel-set-8bpp) endof
    true assert
  endcase
;

( x y val addr width bpp )
: (pixel-set)
  5 pick swap ( x y val addr width x bpp )
;

( base y width x -- pxlval )
: (pixel-get)
  \ Would a jump table be better here?
  case ( base y width x )
    1 of (pixel-get-1bpp) endof
    2 of (pixel-get-2bpp) endof
    4 of (pixel-get-4bpp) endof
    8 of (pixel-get-8bpp) endof
    true assert
  endcase
;
