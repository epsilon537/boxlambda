Estimated FPGA Utilization
==========================

Estimated FPGA Resource Utilization on Arty A7-100T
----------------------------------------------------

| Resources Type | DPRAM | Vera | Ibex | riscv-dbg | LiteDRAM | 2xYM2149+DAC |
|----------------|-------|------|------|-----------|----------|--------------|
|**Slice LUTs**|0|1261|3390|5673|3016|887|
|**Slice Registers**|0|867|911|426|2530|867|
|**Block RAM Tile**|64|40|0|0|0|1|
|**DSPs**|0|0|1|0|2|0|

| Resources Type | PicoRV DMA | sdspi | wbi2c | wbuart | Quad SPI | ps2 keyb.+mouse |
|----------------|------------|-------|-------|--------|----------|-----------|
|**Slice LUTs**|2088|536|393|438|440|410|
|**Slice Registers**|1230|749|324|346|641|370|
|**Block RAM Tile**|1|1|0|0|0|0|
|**DSPs**|0|0|0|0|0|0|

| Resources Type | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|20.00%|22239|63400|35%|
|**Slice Registers**|20.00%|11113|126800|9%|
|**Block RAM Tile**|20.00%|128|135|**95%**|
|**DSPs**|20.00%|4|240|1.67%|

I added a 20% margin overall for the bus fabric and for components I haven't included yet.

Estimated FPGA Resource Utilization on Arty A7-35T
--------------------------------------------------

| Resources Type |  DPRAM | Vera | Ibex | riscv-dbg | LiteDRAM | 2xYM2149+DAC |
|----------------|--------|------|------|-----------|----------|--------------|
|**Slice LUTs**|0|1261|3390|5673|3016|887|
|**Slice Registers**|0|867|911|426|2530|867|
|**Block RAM Tile**|**16**|24|0|0|0|1|
|**DSPs**|0|0|1|0|2|0|

| Resources Type | PicoRV DMA | sdspi | wbi2c | wbuart | Quad SPI | ps2 keyb.+mouse |
|----------------|------------|-------|-------|--------|----------|-----------|
|**Slice LUTs**|2088|536|393|438|440|410|
|**Slice Registers**|1230|749|324|346|641|370|
|**Block RAM Tile**|1|1|0|0|0|0|
|**DSPs**|0|0|0|0|0|0|

| Resources Type | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|**10.00%**|20385|20800|**98%**
|**Slice Registers**|20.00%|11113|41600|27%
|**Block RAM Tile**|**10.00%**|47|50|**94%**
|**DSPs**|20.00%|4|90|4.44%
