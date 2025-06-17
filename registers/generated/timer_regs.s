# --- MTIMER

.equ MTIMER_BASE_ADDR, 0x10020000

# MTIME - Machine-level time counter, low word.
.equ MTIMER_MTIME_ADDR, 0x0

# MTIME.VALUE - Machine-level time counter, low word.
.equ MTIMER_MTIME_VALUE_WIDTH 32
.equ MTIMER_MTIME_VALUE_LSB 0
.equ MTIMER_MTIME_VALUE_MASK 0xffffffff
# MTIMEH - Machine-level time counter, high word.
.equ MTIMER_MTIMEH_ADDR, 0x4

# MTIMEH.VALUE - Machine-level time counter, high word.
.equ MTIMER_MTIMEH_VALUE_WIDTH 32
.equ MTIMER_MTIMEH_VALUE_LSB 0
.equ MTIMER_MTIMEH_VALUE_MASK 0xffffffff
# MTIMECMP - Machine-level time compare, low word.
.equ MTIMER_MTIMECMP_ADDR, 0x8

# MTIMECMP.VALUE - Machine-level time compare, low word.
.equ MTIMER_MTIMECMP_VALUE_WIDTH 32
.equ MTIMER_MTIMECMP_VALUE_LSB 0
.equ MTIMER_MTIMECMP_VALUE_MASK 0xffffffff
# MTIMECMPH - Machine-level time compare, high word.
.equ MTIMER_MTIMECMPH_ADDR, 0xc

# MTIMECMPH.VALUE - Machine-level time compare, high word.
.equ MTIMER_MTIMECMPH_VALUE_WIDTH 32
.equ MTIMER_MTIMECMPH_VALUE_LSB 0
.equ MTIMER_MTIMECMPH_VALUE_MASK 0xffffffff
# MTIMEBLK - Blocking time compare register.
.equ MTIMER_MTIMEBLK_ADDR, 0x10

# MTIMEBLK.VALUE - A write operation to this register blocks the CPU until the lower 8 bits of the MTIME register match the written value.
.equ MTIMER_MTIMEBLK_VALUE_WIDTH 8
.equ MTIMER_MTIMEBLK_VALUE_LSB 0
.equ MTIMER_MTIMEBLK_VALUE_MASK 0xff
