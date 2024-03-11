Estimated FPGA Utilization
==========================

Estimated FPGA Resource Utilization on Arty A7-100T
----------------------------------------------------

| Resources Type | DPRAMs | Vera | Ibex | riscv-dbg | LiteDRAM | 2xYM2149+DAC |
|----------------|--------|------|------|-----------|----------|--------------|
|**Slice LUTs**|1758|1261|3390|5673|3036|887|
|**Slice Registers**|28|867|911|426|2681|867|
|**Block RAM Tile**|64|40|0|0|0|1|
|**DSPs**|0|0|1|0|2|0|

| Resources Type | PicoRV DMA | sdspi | wbi2c | wbuart | Quad SPI | 2xUSB HID |
|----------------|------------|-------|-------|--------|----------|-----------|
|**Slice LUTs**|2522|536|393|438|440|694|
|**Slice Registers**|1359|749|324|346|641|988|
|**Block RAM Tile**|1|1|0|0|0|1|
|**DSPs**|0|0|0|0|0|0|

| Resources Type | Interconnect | Total | Avl. Resources | Pct. Utilization |
|----------------|-------------|--------|----------------|------------------|
|**Slice LUTs**|2846|23874|63400|38%|
|**Slice Registers**|2022|12209|126800|10%|
|**Block RAM Tile**|0|108|135|80%|
|**DSPs**|0|3|240|1%|

Estimated FPGA Resource Utilization on Arty A7-35T
--------------------------------------------------

| Resources Type |  DPRAMs | Vera | Ibex | riscv-dbg | LiteDRAM | 2xYM2149+DAC |
|----------------|---------|------|------|-----------|----------|--------------|
|**Slice LUTs**|422|1261|3390|5673|3036|887|
|**Slice Registers**|10|867|911|426|2681|867|
|**Block RAM Tile**|**16**|24|0|0|0|1|
|**DSPs**|0|0|1|0|2|0|

| Resources Type | PicoRV DMA | sdspi | wbi2c | wbuart | Quad SPI | 2xUSB HID |
|----------------|------------|-------|-------|--------|----------|-----------|
|**Slice LUTs**|2522|536|393|438|440|694|
|**Slice Registers**|1359|749|324|346|641|988|
|**Block RAM Tile**|1|1|0|0|0|1|
|**DSPs**|0|0|0|0|0|0|

| Resources Type | Interconnect | Total | Avl. Resources | Pct. Utilization |
|----------------|--------------|-------|----------------|------------------|
|**Slice LUTs**|2115|21807|20800|**105%**|
|**Slice Registers**|1488|11657|41600|28%|
|**Block RAM Tile**|0|44|50|**88%**|
|**DSPs**|0|4|90|4.44%|

It looks like we might not be able to fit all components in the A7-35T configuration. However, keep in mind that these are estimates. The numbers associated with the individual components don't necessarily add up when you combine these components into a SoC. There will be some gains and losses. The outcome can be also tweaked a bit depending on routing strategy settings, etc. It's going to be a tight fit for sure, but it might just work out. 
