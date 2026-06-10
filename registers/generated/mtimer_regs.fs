\ --- MTIMER

$10020000 constant MTIMER_BASE_ADDR

\ MTIME - Machine-level time counter, low word.
$0 constant MTIMER_MTIME_ADDR

\ MTIME.VALUE - Machine-level time counter, low word.
32 constant MTIMER_MTIME_VALUE_WIDTH
0 constant MTIMER_MTIME_VALUE_LSB
$ffffffff constant MTIMER_MTIME_VALUE_MASK
\ MTIMEH - Machine-level time counter, high word.
$4 constant MTIMER_MTIMEH_ADDR

\ MTIMEH.VALUE - Machine-level time counter, high word.
32 constant MTIMER_MTIMEH_VALUE_WIDTH
0 constant MTIMER_MTIMEH_VALUE_LSB
$ffffffff constant MTIMER_MTIMEH_VALUE_MASK
\ MTIMECMP - Machine-level time compare, low word.
$8 constant MTIMER_MTIMECMP_ADDR

\ MTIMECMP.VALUE - Machine-level time compare, low word.
32 constant MTIMER_MTIMECMP_VALUE_WIDTH
0 constant MTIMER_MTIMECMP_VALUE_LSB
$ffffffff constant MTIMER_MTIMECMP_VALUE_MASK
\ MTIMECMPH - Machine-level time compare, high word.
$c constant MTIMER_MTIMECMPH_ADDR

\ MTIMECMPH.VALUE - Machine-level time compare, high word.
32 constant MTIMER_MTIMECMPH_VALUE_WIDTH
0 constant MTIMER_MTIMECMPH_VALUE_LSB
$ffffffff constant MTIMER_MTIMECMPH_VALUE_MASK
\ MTIMEBLK - Blocking time compare register.
$10 constant MTIMER_MTIMEBLK_ADDR

\ MTIMEBLK.VALUE - A write operation to this register blocks the CPU until the lower 8 bits of the MTIME register match the written value.
8 constant MTIMER_MTIMEBLK_VALUE_WIDTH
0 constant MTIMER_MTIMEBLK_VALUE_LSB
$ff constant MTIMER_MTIMEBLK_VALUE_MASK
