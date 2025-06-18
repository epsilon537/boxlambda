---
hide:
  - toc
---

## SDSPI

- **SDSPI Software Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/sdspi/](https://github.com/epsilon537/boxlambda/tree/master/sw/components/sdspi)

The SDSPI software components provides the following APIs:

- [sdspi_hal.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/sdspi/sdspi_hal.h): This is just the SDSPI Register Access Layer with some additional helper definitions.
- [sdcard.h](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/sdspi/sdcard.h): A higher level API for SD Card interaction, used by [FatFs](sw_comp_fat_fs.md).

The SDCard module based on ZipCPU's SDCard implementation in his [SDSPI repo](https://github.com/epsilon537/sdspi).

### SDSPI Test

The SDSPI Test program tests the SDSPI core using the SDSPI HAL. It does not use
the SDCard API.

