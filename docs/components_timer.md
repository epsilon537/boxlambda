## Timer

The Timer module is part of the **Ibex** RISCV repo. See the [Ibex Component page](components_ibex.md) for the Ibex repo link and submodule path.

- **Timer Component in the BoxLambda Directory Tree**:
    [boxlambda/gw/components/wb_timer](https://github.com/epsilon537/boxlambda/tree/master/gw/components/wb_timer)

- **Timer Module Top-Level**:
    [boxlambda/gw/components/wb_timer/rtl/wb_timer.sv](https://github.com/epsilon537/boxlambda/tree/master/gw/components/wb_timer/rtl/wb_timer.sv)

- **Timer Spec**: See section 3.1.15 (*Machine Timer Registers*) of the RISC-V Privileged Specification:
    [https://riscv.org/wp-content/uploads/2017/05/riscv-privileged-v1.10.pdf](https://riscv.org/wp-content/uploads/2017/05/riscv-privileged-v1.10.pdf)

The Timer component is a basic timer peripheral capable of generating interrupts based on the RISC-V Machine Timer Registers.

### Timer Clock Frequency

The Timer module is part of the 50MHz System Clock Domain.

