Estimated FPGA Utilization
==========================

Estimated FPGA Resource Utilization on Nexys A7-100T
----------------------------------------------------

| Resources Type | DPRAM | Vera | Ibex | riscv-dbg | MIG | Dual JT49 | Praxos DMA |
|----------------|-------|------|------|-----------|-----|-----------|------------|
|**Slice LUTs**|0|2122|3390|5673|416|554|380|
|**Slice Registers**|0|1441|911|426|5060|622|167|
|**Block RAM Tile**|64|41|0|0|1|0.5|
|**DSPs**|0|2|1|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | ps2 keyb. | ps2 mouse |
|----------------|-------|-------|--------|----------|-----------|-----------|
|**Slice LUTs**|536|393|438|440|205|205|
|**Slice Registers**|324|114|346|641|185|185|
|**Block RAM Tile**|1|0|0|0|0|0|
|**DSPs**|0|0|0|0|

| Resources Type | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|20.00%|17702|63400|27.92%|
|**Slice Registers**|20.00%|13268|126800|10.46%|
|**Block RAM Tile**|20.00%|129|135|95.56%|
|**DSPs**|20.00%|3.6|240|1.50%|

I added a 20% margin overall for the bus fabric and for components I haven't included yet.

Estimated FPGA Resource Utilization on Arty A7-35T
--------------------------------------------------

| Resources Type |  DPRAM | Vera | Ibex | riscv-dbg | MIG | Dual JT49 | Praxos DMA |
|----------------|--------|------|------|-----------|-----|-----------|------------|
|**Slice LUTs**|0|2122|3390|5673|416|554|380|205|205
|**Slice Registers**|0|1441|911|426|5060|622|167|185|185
|**Block RAM Tile**|**16**|25|0|0|1|0.5|0|0
|**DSPs**|0|2|1|0|0|0|0|0

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | ps2 keyb. | ps2 mouse |
|----------------|-------|-------|--------|----------|-----------|-----------|
|**Slice LUTs**|536|393|438|440|205|205
|**Slice Registers**|749|324|346|641|185|185
|**Block RAM Tile**|1|0|0|0|0|0
|**DSPs**|0|0|0|0|0|0

| Resources Type | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|20.00%|17702|20800|85.11%
|**Slice Registers**|20.00%|13268|41600|31.90%
|**Block RAM Tile**|**10.00%**|48|50|**95.70%**
|**DSPs**|20.00%|4|90|4.00%
