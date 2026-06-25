\ --- MTIMER
\ --- 32-bit register bitfield accessors

$10020000 constant MTIMER_BASE_ADDR


\ MTIME - Machine-level time counter, low word.
MTIMER_BASE_ADDR $0 + constant MTIMER_MTIME_ADDR

\ -- Address Mask Offset
MTIMER_MTIME_ADDR $ffffffff #0 bitfield@ MTIMER_MTIME_VALUE@
MTIMER_MTIME_ADDR $ffffffff #0 bitfield! MTIMER_MTIME_VALUE!

\ MTIMEH - Machine-level time counter, high word.
MTIMER_BASE_ADDR $4 + constant MTIMER_MTIMEH_ADDR

\ -- Address Mask Offset
MTIMER_MTIMEH_ADDR $ffffffff #0 bitfield@ MTIMER_MTIMEH_VALUE@
MTIMER_MTIMEH_ADDR $ffffffff #0 bitfield! MTIMER_MTIMEH_VALUE!

\ MTIMECMP - Machine-level time compare, low word.
MTIMER_BASE_ADDR $8 + constant MTIMER_MTIMECMP_ADDR

\ -- Address Mask Offset
MTIMER_MTIMECMP_ADDR $ffffffff #0 bitfield@ MTIMER_MTIMECMP_VALUE@
MTIMER_MTIMECMP_ADDR $ffffffff #0 bitfield! MTIMER_MTIMECMP_VALUE!

\ MTIMECMPH - Machine-level time compare, high word.
MTIMER_BASE_ADDR $c + constant MTIMER_MTIMECMPH_ADDR

\ -- Address Mask Offset
MTIMER_MTIMECMPH_ADDR $ffffffff #0 bitfield@ MTIMER_MTIMECMPH_VALUE@
MTIMER_MTIMECMPH_ADDR $ffffffff #0 bitfield! MTIMER_MTIMECMPH_VALUE!

\ MTIMEBLK - Blocking time compare register.
MTIMER_BASE_ADDR $10 + constant MTIMER_MTIMEBLK_ADDR

\ -- Address Mask Offset
MTIMER_MTIMEBLK_ADDR $ff #0 bitfield@ MTIMER_MTIMEBLK_VALUE@
MTIMER_MTIMEBLK_ADDR $ff #0 bitfield! MTIMER_MTIMEBLK_VALUE!
