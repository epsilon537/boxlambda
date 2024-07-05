## Timer, UART, and GPIO Interrupt Test

### The Timer, UART, and GPIO Interrupt Test on Verilator

Build the *timer_uart_gpio_irqs* gateware project:

```
cd build/sim-a7-100/gw/projects/timer_uart_gpio_irqs
make timer_uart_gpio_irqs_sim_sw
```

Execute the generated Verilator model:

```
./Vmodel
SIM: DUT: Load PicoRV Program picorv_irq_in_out.
SIM: DUT: Taking PicoRV out of reset...
SIM: DUT: Enabling Ibex IRQs
SIM: DUT: Setting timer...
SIM: DUT: Timer irq fired.
SIM: DUT: Timer irq fired.
SIM: DUT: Timer Test Successful.
SIM: SIM: Timer test successful. Time = 3208396
SIM: DUT: Testing UART TX IRQs...
SIM: DUT: 0123456789
SIM: DUT: UART TX IRQ test successful.
SIM: SIM: UART TX IRQ test successful. Time = 4319436
SIM: DUT: Testing UART RX IRQs...
SIM: DUT: Please enter a character.
SIM: SIM: inserting uart character (1). Time = 5187436
SIM: DUT: UART RX IRQ received.
SIM: DUT: Received character: a
SIM: DUT: Please enter another character.
SIM: SIM: inserting uart character (2). Time = 6526280
SIM: DUT: UART RX IRQ received.
SIM: DUT: Received character: b
SIM: DUT: Please enter 8 characters. They will be echoed when all 8 characters are received.
SIM: SIM: inserting character sequence. Time = 8750484
SIM: SIM: finished inserting uart string (1).
SIM: DUT: UART RX FIFO IRQ received.
SIM: DUT: Received character: H
SIM: DUT: Received character: e
...
SIM: DUT: Received character: SIM: DUT: Please enter 8 characters again. They will be echoed when all 8 characters are received.
SIM: SIM: Inserting character sequence again. Time = 15870184
SIM: SIM: finished inserting uart string (2).
SIM: DUT: UART RX FIFO IRQ received.
SIM: DUT: Received character: H
SIM: DUT: Received character: e
...
SIM: DUT: Received character: SIM: DUT: UART RX IRQ Test Successful.
SIM: SIM: UART RX IRQ tests successful. Time = 21948348
SIM: DUT: Testing GPIO IRQs...
SIM: DUT: Push some buttons. The LEDS should track the button presses/releases.
SIM: SIM: Testing GPIO. Time = 23528108
SIM: SIM: LED 0 turned on.
SIM: SIM: LED 0 turned off.
SIM: SIM: LED 1 turned on.
SIM: SIM: LED 1 turned off.
SIM: SIM: GPIO test OK.
SIM: SIM: GPIO test successful. Time = 23536664
SIM: Test passed.
```

### The Timer, UART, and GPIO Interrupt Test on FPGA

If you're running on WSL, check BoxLambda's documentation [On WSL](https://boxlambda.readthedocs.io/en/latest/installation/#on-wsl) section.

Connect a terminal program to Arty's USB serial port. **Settings: 115200 8N1**.

Build the *timer_uart_gpio_irqs* gateware project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):

```
cd build/arty-a7-100/gw/projects/timer_uart_gpio_irqs
make timer_uart_gpio_irqs_bit_sw
```

Download the generated bitstream file to the Arty A7:

```
make timer_uart_gpio_irqs_load
```

Follow the prompts on the serial port terminal. Push the Arty's buttons or enter characters into the terminal when prompted.

