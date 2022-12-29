Estimated FPGA Utilization
==========================

Estimated FPGA Resource Utilization on Arty A7-100T
----------------------------------------------------

| Resources Type | DPRAM | Vera | Ibex | riscv-dbg | LiteDRAM | Dual JT49 | Praxos DMA |
|----------------|-------|------|------|-----------|----------|-----------|------------|
|**Slice LUTs**|0|2122|3390|5673|3016|554|380|
|**Slice Registers**|0|1441|911|426|2530|622|167|
|**Block RAM Tile**|64|41|0|0|0|0.5|
|**DSPs**|0|2|1|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | ps2 keyb. | ps2 mouse |
|----------------|-------|-------|--------|----------|-----------|-----------|
|**Slice LUTs**|536|393|438|440|205|205|
|**Slice Registers**|324|114|346|641|185|185|
|**Block RAM Tile**|1|0|0|0|0|0|
|**DSPs**|0|0|0|0|

| Resources Type | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|20.00%|20833|63400|32.86%|
|**Slice Registers**|20.00%|9470|126800|7.46%|
|**Block RAM Tile**|20.00%|128|135|**94.81%**|
|**DSPs**|20.00%|5|240|2.08%|

I added a 20% margin overall for the bus fabric and for components I haven't included yet.

Estimated FPGA Resource Utilization on Arty A7-35T
--------------------------------------------------

| Resources Type |  DPRAM | Vera | Ibex | riscv-dbg | LiteDRAM | Dual JT49 | Praxos DMA |
|----------------|--------|------|------|-----------|----------|-----------|------------|
|**Slice LUTs**|0|2122|3390|5673|3016|554|380|205|205
|**Slice Registers**|0|1441|911|426|2530|622|167|185|185
|**Block RAM Tile**|**16**|25|0|0|0|0.5|0|0
|**DSPs**|0|2|1|0|0|0|0|0

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | ps2 keyb. | ps2 mouse |
|----------------|-------|-------|--------|----------|-----------|-----------|
|**Slice LUTs**|536|393|438|440|205|205
|**Slice Registers**|749|324|346|641|185|185
|**Block RAM Tile**|1|0|0|0|0|0
|**DSPs**|0|0|0|0|0|0

| Resources Type | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|**10.00%**|19538|20800|**93.93%**
|**Slice Registers**|20.00%|10676|41600|25.66%
|**Block RAM Tile**|**10.00%**|48|50|**96.00%**
|**DSPs**|20.00%|5|90|5.56%
