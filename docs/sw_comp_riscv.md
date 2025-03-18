---
hide:
  - toc
---

## RISC-V

### MCycle

The MCycle API gives access to the Ibex processor's cycle counter. It also provides a cycle counter-based implementation of the micro-sleep function *usleep*.

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/riscv/mcycle.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/riscv/mcycle.h)

### CSRs

RISC-V CSRs, Control and Status Registers, are special registers through which the programmer can configure the CPU and query its status. CSR registers live in a separate address space, accessed through special instructions such as:

- *csrr a0, mstatus*: read mstatus CSR contents into register a0
- *csrw mtvec, a1*: write register a1 contents into the mtvec CSR.

For a complete list of CSRs, see the [Ibex Controls and Status Registers](https://ibex-core.readthedocs.io/en/latest/03_reference/cs_registers.html#cs-registers) page.

To be able to access the CSRs easily from C/C++ I'm using Five Embeddev's *riscv-csr-access* library:

[https://github.com/five-embeddev/riscv-csr-access](https://github.com/five-embeddev/riscv-csr-access)

The library consists of a single .h file, which I copied into BoxLambda's *riscv* software component directory:

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/riscv/riscv-csr.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/riscv/riscv-csr.h)

