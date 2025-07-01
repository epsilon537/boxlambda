---
hide:
  - toc
---

## VERA Hardware Access Layer

**VERA HAL**: [sw/components/vera/vera_hal.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/vera/vera_hal.h)

The VERA HAL is a very thin Hardware Access Layer, mapping directly to VERA's
registers and memories. A few helper functions are provided such as `palette_ram_wr()`, `sprite_attr_wr()`, and `vram_wr_byte()`.

