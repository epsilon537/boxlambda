# --- GPIO

.equ GPIO_BASE_ADDR, 0x10000100

# RGPIO_IN - Latched value of general-purpose input pins.
.equ GPIO_RGPIO_IN_ADDR, 0x0

# RGPIO_IN.PINS - Latched value of general-purpose input pins.
.equ GPIO_RGPIO_IN_PINS_WIDTH 32
.equ GPIO_RGPIO_IN_PINS_LSB 0
.equ GPIO_RGPIO_IN_PINS_MASK 0xffffffff
# RGPIO_OUT - General-purpose output pin values.
.equ GPIO_RGPIO_OUT_ADDR, 0x4

# RGPIO_OUT.PINS - General-purpose output pin values.
.equ GPIO_RGPIO_OUT_PINS_WIDTH 32
.equ GPIO_RGPIO_OUT_PINS_LSB 0
.equ GPIO_RGPIO_OUT_PINS_MASK 0xffffffff
# RGPIO_OE - General-purpose pins output enables.
.equ GPIO_RGPIO_OE_ADDR, 0x8

# RGPIO_OE.PINS - General-purpose pins output enables.
.equ GPIO_RGPIO_OE_PINS_WIDTH 32
.equ GPIO_RGPIO_OE_PINS_LSB 0
.equ GPIO_RGPIO_OE_PINS_MASK 0xffffffff
# RGPIO_INTE - General-purpose pin interrupt enables.
.equ GPIO_RGPIO_INTE_ADDR, 0xc

# RGPIO_INTE.PINS - General-purpose pins interrupt enables.
.equ GPIO_RGPIO_INTE_PINS_WIDTH 32
.equ GPIO_RGPIO_INTE_PINS_LSB 0
.equ GPIO_RGPIO_INTE_PINS_MASK 0xffffffff
# RGPIO_PTRIG - Trigger IRQ on positive edge.
.equ GPIO_RGPIO_PTRIG_ADDR, 0x10

# RGPIO_PTRIG.PINS - Trigger IRQ on positive edge if set, on negative edge if cleared.
.equ GPIO_RGPIO_PTRIG_PINS_WIDTH 32
.equ GPIO_RGPIO_PTRIG_PINS_LSB 0
.equ GPIO_RGPIO_PTRIG_PINS_MASK 0xffffffff
# RGPIO_CTRL_STATUS - GPIO control and status register
.equ GPIO_RGPIO_CTRL_STATUS_ADDR, 0x18

# RGPIO_CTRL_STATUS.INTE - Interrupt enabled
.equ GPIO_RGPIO_CTRL_STATUS_INTE_WIDTH 1
.equ GPIO_RGPIO_CTRL_STATUS_INTE_LSB 0
.equ GPIO_RGPIO_CTRL_STATUS_INTE_MASK 0x1
# RGPIO_CTRL_STATUS.INTS - Interrupt status
.equ GPIO_RGPIO_CTRL_STATUS_INTS_WIDTH 1
.equ GPIO_RGPIO_CTRL_STATUS_INTS_LSB 1
.equ GPIO_RGPIO_CTRL_STATUS_INTS_MASK 0x2
# RGPIO_INTS - GPIO interrupt status register.
.equ GPIO_RGPIO_INTS_ADDR, 0x1c

# RGPIO_INTS.PINS - Interrupt status - Write 0 to clear.
.equ GPIO_RGPIO_INTS_PINS_WIDTH 32
.equ GPIO_RGPIO_INTS_PINS_LSB 0
.equ GPIO_RGPIO_INTS_PINS_MASK 0xffffffff
# RGPIO_ECLK - Latch on gp_clk input signal.
.equ GPIO_RGPIO_ECLK_ADDR, 0x20

# RGPIO_ECLK.PINS - When set, the gp_clk input signal is used to latch pin.
.equ GPIO_RGPIO_ECLK_PINS_WIDTH 32
.equ GPIO_RGPIO_ECLK_PINS_LSB 0
.equ GPIO_RGPIO_ECLK_PINS_MASK 0xffffffff
# RGPIO_NEC - Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set.
.equ GPIO_RGPIO_NEC_ADDR, 0x24

# RGPIO_NEC.PINS - When set, gp_clk is active on negative edge, when cleared on positive edge.
.equ GPIO_RGPIO_NEC_PINS_WIDTH 32
.equ GPIO_RGPIO_NEC_PINS_LSB 0
.equ GPIO_RGPIO_NEC_PINS_MASK 0xffffffff
