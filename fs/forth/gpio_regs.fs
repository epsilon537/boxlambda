\ --- GPIO
\ --- 32-bit register bitfield accessors

$10000100 constant GPIO_BASE_ADDR


\ RGPIO_IN - Latched value of general-purpose input pins.
GPIO_BASE_ADDR $0 + constant GPIO_RGPIO_IN_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_IN_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_IN_PINS@

\ RGPIO_OUT - General-purpose output pin values.
GPIO_BASE_ADDR $4 + constant GPIO_RGPIO_OUT_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_OUT_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_OUT_PINS@
GPIO_RGPIO_OUT_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_OUT_PINS!

\ RGPIO_OE - General-purpose pins output enables.
GPIO_BASE_ADDR $8 + constant GPIO_RGPIO_OE_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_OE_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_OE_PINS@
GPIO_RGPIO_OE_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_OE_PINS!

\ RGPIO_INTE - General-purpose pin interrupt enables.
GPIO_BASE_ADDR $c + constant GPIO_RGPIO_INTE_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_INTE_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_INTE_PINS@
GPIO_RGPIO_INTE_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_INTE_PINS!

\ RGPIO_PTRIG - Trigger IRQ on positive edge.
GPIO_BASE_ADDR $10 + constant GPIO_RGPIO_PTRIG_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_PTRIG_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_PTRIG_PINS@
GPIO_RGPIO_PTRIG_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_PTRIG_PINS!

\ RGPIO_CTRL_STATUS - GPIO control and status register
GPIO_BASE_ADDR $18 + constant GPIO_RGPIO_CTRL_STATUS_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_CTRL_STATUS_ADDR $1 #0 bitfield@ GPIO_RGPIO_CTRL_STATUS_INTE@
GPIO_RGPIO_CTRL_STATUS_ADDR $1 #0 bitfield! GPIO_RGPIO_CTRL_STATUS_INTE!
GPIO_RGPIO_CTRL_STATUS_ADDR $2 #1 bitfield@ GPIO_RGPIO_CTRL_STATUS_INTS@

\ RGPIO_INTS - GPIO interrupt status register.
GPIO_BASE_ADDR $1c + constant GPIO_RGPIO_INTS_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_INTS_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_INTS_PINS@
GPIO_RGPIO_INTS_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_INTS_PINS!

\ RGPIO_ECLK - Latch on gp_clk input signal.
GPIO_BASE_ADDR $20 + constant GPIO_RGPIO_ECLK_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_ECLK_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_ECLK_PINS@
GPIO_RGPIO_ECLK_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_ECLK_PINS!

\ RGPIO_NEC - Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set.
GPIO_BASE_ADDR $24 + constant GPIO_RGPIO_NEC_ADDR

\ -- Address Mask Offset
GPIO_RGPIO_NEC_ADDR $ffffffff #0 bitfield@ GPIO_RGPIO_NEC_PINS@
GPIO_RGPIO_NEC_ADDR $ffffffff #0 bitfield! GPIO_RGPIO_NEC_PINS!
