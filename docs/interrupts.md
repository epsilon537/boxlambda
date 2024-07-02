Interrupts
----------

The CPU supports the following interrupts (taken from [https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html)):

**Ibex Interrupts:**

| Interrupt Input Signal  | ID    | Description                                      |
-------------------------|-------|--------------------------------------------------|
| ``irq_nm_i``            | 31    | Non-maskable interrupt (NMI)                     |
| ``irq_fast_i[14:0]``    | 30:16 | 15 fast, local interrupts                        |
| ``irq_external_i``      | 11    | Connected to platform-level interrupt controller |
| ``irq_timer_i``         | 7     | Connected to timer module                        |
| ``irq_software_i``      | 3     | Connected to memory-mapped (inter-processor) interrupt register    |

### The Timer

The RISC-V spec includes a timer specification: RISC-V Machine Timer Registers (see RISC-V Privileged Specification, version 1.11, Section 3.1.10). The Ibex GitHub repository contains a compliant implementation as part of the *Simple System* example:

[https://github.com/epsilon537/ibex/tree/master/examples/simple_system](https://github.com/epsilon537/ibex/tree/master/examples/simple_system)

The Timer module flags interrupts via signal *irq_timer_i*. The CPU sees this as IRQ ID 7.

### The Fast Local Interrupts

We can freely assign 15 local interrupts. I've got the following list:

- 1 interrupt line per Reconfigurable Module (RM), so 3 in total. The default RMs are VERA and a Dual JT49. VERA uses one interrupt line, JT49 uses none.
- 1 interrupt line each for:
    - UART
    - GPIO
    - SDSPI
    - USB_HID_0
    - USB_HID_1
    - I2C
    - PicoRV DMA
    - ICAP
    - DFX Controller

  That's 12 interrupts in total.

The interrupts are serviced in order of priority, the highest number being the highest priority.

I have ordered the Fast Local interrupts as follows:

**Fast Local Interrupt Assignments:**

| Interrupt Input Signal  | ID    | Description                             |
|-------------------------|-------|-----------------------------------------|
| ``irq_fast_i[14]``      | 30    | RM_2 interrupt (Default: not assigned)  |
| ``irq_fast_i[13]``      | 29    | RM_1 interrupt (Default: VERA IRQ)      |
| ``irq_fast_i[12]``      | 28    | RM_0 interrupt (Default: not assigned)  |
| ``irq_fast_i[11]``      | 27    | PICORV DMAC IRQ                         |
| ``irq_fast_i[10]``      | 26    | SDSPI IRQ                               |
| ``irq_fast_i[9]``       | 25    | GPIO                                    |
| ``irq_fast_i[8]``       | 24    | usb_hid_1 IRQ                           |
| ``irq_fast_i[7]``       | 23    | usb_hid_0 IRQ                           |
| ``irq_fast_i[6]``       | 22    | I2C IRQ                                 |
| ``irq_fast_i[5]``       | 21    | UART                                    |
| ``irq_fast_i[4]``       | 20    | not assigned                            |
| ``irq_fast_i[3]``       | 19    | not assigned                            |
| ``irq_fast_i[2]``       | 18    | not assigned                            |
| ``irq_fast_i[1]``       | 17    | DFX Controller IRQ                      |
| ``irq_fast_i[0]``       | 16    | ICAP IRQ                                |

### The Platform Level Interrupt Controller.

One interrupt line is reserved to connect an external interrupt controller. I don't have any use for it right now, however, so I'm going to leave this unassigned for the time being.

