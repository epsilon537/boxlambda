---
hide:
  - toc
---

## PicoRV DMA Hardware Access Layer

- **PicoRV HAL Software Component in the BoxLambda Directory Tree**:
  [sw/components/picorv_dma](https://github.com/epsilon537/boxlambda/tree/master/sw/components/picorv_dma)

There are two HALs in this directory:

- **picorv_dma_hal.h**: This is a low-level HAL interface to the PicoRV DMA core for the Ibex Host Processor. It defines accessors for the PicoRV DMA's register interface and a function for loading a micro-program into the PicoRV DMA Program Memory.
- **picoasm_hal.h**: This is a simple HAL for PicoRV assembly programs. It defines names for HIR and Burst registers, etc.


