( pxlval base y width x -- )
: (pixel-set-1bpp)
  -rot (1bpp-pixel-byte-ptr) ( pxlval ptr )
  setbit ( )
;
