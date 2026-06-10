\ --- GPIO

$10000100 constant GPIO_BASE_ADDR

\ RGPIO_IN - Latched value of general-purpose input pins.
$0 constant GPIO_RGPIO_IN_ADDR

\ RGPIO_IN.PINS - Latched value of general-purpose input pins.
32 constant GPIO_RGPIO_IN_PINS_WIDTH
0 constant GPIO_RGPIO_IN_PINS_LSB
$ffffffff constant GPIO_RGPIO_IN_PINS_MASK
\ RGPIO_OUT - General-purpose output pin values.
$4 constant GPIO_RGPIO_OUT_ADDR

\ RGPIO_OUT.PINS - General-purpose output pin values.
32 constant GPIO_RGPIO_OUT_PINS_WIDTH
0 constant GPIO_RGPIO_OUT_PINS_LSB
$ffffffff constant GPIO_RGPIO_OUT_PINS_MASK
\ RGPIO_OE - General-purpose pins output enables.
$8 constant GPIO_RGPIO_OE_ADDR

\ RGPIO_OE.PINS - General-purpose pins output enables.
32 constant GPIO_RGPIO_OE_PINS_WIDTH
0 constant GPIO_RGPIO_OE_PINS_LSB
$ffffffff constant GPIO_RGPIO_OE_PINS_MASK
\ RGPIO_INTE - General-purpose pin interrupt enables.
$c constant GPIO_RGPIO_INTE_ADDR

\ RGPIO_INTE.PINS - General-purpose pins interrupt enables.
32 constant GPIO_RGPIO_INTE_PINS_WIDTH
0 constant GPIO_RGPIO_INTE_PINS_LSB
$ffffffff constant GPIO_RGPIO_INTE_PINS_MASK
\ RGPIO_PTRIG - Trigger IRQ on positive edge.
$10 constant GPIO_RGPIO_PTRIG_ADDR

\ RGPIO_PTRIG.PINS - Trigger IRQ on positive edge if set, on negative edge if cleared.
32 constant GPIO_RGPIO_PTRIG_PINS_WIDTH
0 constant GPIO_RGPIO_PTRIG_PINS_LSB
$ffffffff constant GPIO_RGPIO_PTRIG_PINS_MASK
\ RGPIO_CTRL_STATUS - GPIO control and status register
$18 constant GPIO_RGPIO_CTRL_STATUS_ADDR

\ RGPIO_CTRL_STATUS.INTE - Interrupt enabled
1 constant GPIO_RGPIO_CTRL_STATUS_INTE_WIDTH
0 constant GPIO_RGPIO_CTRL_STATUS_INTE_LSB
$1 constant GPIO_RGPIO_CTRL_STATUS_INTE_MASK
\ RGPIO_CTRL_STATUS.INTS - Interrupt status
1 constant GPIO_RGPIO_CTRL_STATUS_INTS_WIDTH
1 constant GPIO_RGPIO_CTRL_STATUS_INTS_LSB
$2 constant GPIO_RGPIO_CTRL_STATUS_INTS_MASK
\ RGPIO_INTS - GPIO interrupt status register.
$1c constant GPIO_RGPIO_INTS_ADDR

\ RGPIO_INTS.PINS - Interrupt status - Write 0 to clear.
32 constant GPIO_RGPIO_INTS_PINS_WIDTH
0 constant GPIO_RGPIO_INTS_PINS_LSB
$ffffffff constant GPIO_RGPIO_INTS_PINS_MASK
\ RGPIO_ECLK - Latch on gp_clk input signal.
$20 constant GPIO_RGPIO_ECLK_ADDR

\ RGPIO_ECLK.PINS - When set, the gp_clk input signal is used to latch pin.
32 constant GPIO_RGPIO_ECLK_PINS_WIDTH
0 constant GPIO_RGPIO_ECLK_PINS_LSB
$ffffffff constant GPIO_RGPIO_ECLK_PINS_MASK
\ RGPIO_NEC - Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set.
$24 constant GPIO_RGPIO_NEC_ADDR

\ RGPIO_NEC.PINS - When set, gp_clk is active on negative edge, when cleared on positive edge.
32 constant GPIO_RGPIO_NEC_PINS_WIDTH
0 constant GPIO_RGPIO_NEC_PINS_LSB
$ffffffff constant GPIO_RGPIO_NEC_PINS_MASK
