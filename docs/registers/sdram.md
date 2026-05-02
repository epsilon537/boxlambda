# SDRAM

## Register Listing for SDRAM

| Register                                                      | Address                                     |
|---------------------------------------------------------------|---------------------------------------------|
| [SDRAM_DFII_CONTROL](#sdram-dfii-control)                     | [0x00001000](#sdram-dfii-control)           |
| [SDRAM_DFII_PI0_COMMAND](#sdram-dfii-pi0-command)             | [0x00001004](#sdram-dfii-pi0-command)       |
| [SDRAM_DFII_PI0_COMMAND_ISSUE](#sdram-dfii-pi0-command-issue) | [0x00001008](#sdram-dfii-pi0-command-issue) |
| [SDRAM_DFII_PI0_ADDRESS](#sdram-dfii-pi0-address)             | [0x0000100c](#sdram-dfii-pi0-address)       |
| [SDRAM_DFII_PI0_BADDRESS](#sdram-dfii-pi0-baddress)           | [0x00001010](#sdram-dfii-pi0-baddress)      |
| [SDRAM_DFII_PI0_WRDATA](#sdram-dfii-pi0-wrdata)               | [0x00001014](#sdram-dfii-pi0-wrdata)        |
| [SDRAM_DFII_PI0_RDDATA](#sdram-dfii-pi0-rddata)               | [0x00001018](#sdram-dfii-pi0-rddata)        |
| [SDRAM_DFII_PI1_COMMAND](#sdram-dfii-pi1-command)             | [0x0000101c](#sdram-dfii-pi1-command)       |
| [SDRAM_DFII_PI1_COMMAND_ISSUE](#sdram-dfii-pi1-command-issue) | [0x00001020](#sdram-dfii-pi1-command-issue) |
| [SDRAM_DFII_PI1_ADDRESS](#sdram-dfii-pi1-address)             | [0x00001024](#sdram-dfii-pi1-address)       |
| [SDRAM_DFII_PI1_BADDRESS](#sdram-dfii-pi1-baddress)           | [0x00001028](#sdram-dfii-pi1-baddress)      |
| [SDRAM_DFII_PI1_WRDATA](#sdram-dfii-pi1-wrdata)               | [0x0000102c](#sdram-dfii-pi1-wrdata)        |
| [SDRAM_DFII_PI1_RDDATA](#sdram-dfii-pi1-rddata)               | [0x00001030](#sdram-dfii-pi1-rddata)        |
| [SDRAM_DFII_PI2_COMMAND](#sdram-dfii-pi2-command)             | [0x00001034](#sdram-dfii-pi2-command)       |
| [SDRAM_DFII_PI2_COMMAND_ISSUE](#sdram-dfii-pi2-command-issue) | [0x00001038](#sdram-dfii-pi2-command-issue) |
| [SDRAM_DFII_PI2_ADDRESS](#sdram-dfii-pi2-address)             | [0x0000103c](#sdram-dfii-pi2-address)       |
| [SDRAM_DFII_PI2_BADDRESS](#sdram-dfii-pi2-baddress)           | [0x00001040](#sdram-dfii-pi2-baddress)      |
| [SDRAM_DFII_PI2_WRDATA](#sdram-dfii-pi2-wrdata)               | [0x00001044](#sdram-dfii-pi2-wrdata)        |
| [SDRAM_DFII_PI2_RDDATA](#sdram-dfii-pi2-rddata)               | [0x00001048](#sdram-dfii-pi2-rddata)        |
| [SDRAM_DFII_PI3_COMMAND](#sdram-dfii-pi3-command)             | [0x0000104c](#sdram-dfii-pi3-command)       |
| [SDRAM_DFII_PI3_COMMAND_ISSUE](#sdram-dfii-pi3-command-issue) | [0x00001050](#sdram-dfii-pi3-command-issue) |
| [SDRAM_DFII_PI3_ADDRESS](#sdram-dfii-pi3-address)             | [0x00001054](#sdram-dfii-pi3-address)       |
| [SDRAM_DFII_PI3_BADDRESS](#sdram-dfii-pi3-baddress)           | [0x00001058](#sdram-dfii-pi3-baddress)      |
| [SDRAM_DFII_PI3_WRDATA](#sdram-dfii-pi3-wrdata)               | [0x0000105c](#sdram-dfii-pi3-wrdata)        |
| [SDRAM_DFII_PI3_RDDATA](#sdram-dfii-pi3-rddata)               | [0x00001060](#sdram-dfii-pi3-rddata)        |

### SDRAM_DFII_CONTROL

Address: 0x00001000 + 0x0 = 0x00001000

> Control DFI signals common to all phases

| Field   | Name    | Description                                                                                                                                                                         |
|---------|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [0]     | SEL     | | Value   | Description                 |<br/>|---------|-----------------------------|<br/>| `0b0`   | Software (CPU) control.     |<br/>| `0b1`   | Hardware control (default). | |
| [1]     | CKE     | DFI clock enable bus                                                                                                                                                                |
| [2]     | ODT     | DFI on-die termination bus                                                                                                                                                          |
| [3]     | RESET_N | DFI clock reset bus                                                                                                                                                                 |

### SDRAM_DFII_PI0_COMMAND

Address: 0x00001000 + 0x4 = 0x00001004

> Control DFI signals on a single phase

| Field   | Name      | Description                              |
|---------|-----------|------------------------------------------|
| [0]     | CS        | DFI chip select bus                      |
| [1]     | WE        | DFI write enable bus                     |
| [2]     | CAS       | DFI column address strobe bus            |
| [3]     | RAS       | DFI row address strobe bus               |
| [4]     | WREN      | DFI write data enable bus                |
| [5]     | RDEN      | DFI read data enable bus                 |
| [6]     | CS_TOP    | DFI chip select bus for top half only    |
| [7]     | CS_BOTTOM | DFI chip select bus for bottom half only |

### SDRAM_DFII_PI0_COMMAND_ISSUE

Address: 0x00001000 + 0x8 = 0x00001008

### SDRAM_DFII_PI0_ADDRESS

Address: 0x00001000 + 0xc = 0x0000100c

> DFI address bus

### SDRAM_DFII_PI0_BADDRESS

Address: 0x00001000 + 0x10 = 0x00001010

> DFI bank address bus

### SDRAM_DFII_PI0_WRDATA

Address: 0x00001000 + 0x14 = 0x00001014

> DFI write data bus

### SDRAM_DFII_PI0_RDDATA

Address: 0x00001000 + 0x18 = 0x00001018

> DFI read data bus

### SDRAM_DFII_PI1_COMMAND

Address: 0x00001000 + 0x1c = 0x0000101c

> Control DFI signals on a single phase

| Field   | Name      | Description                              |
|---------|-----------|------------------------------------------|
| [0]     | CS        | DFI chip select bus                      |
| [1]     | WE        | DFI write enable bus                     |
| [2]     | CAS       | DFI column address strobe bus            |
| [3]     | RAS       | DFI row address strobe bus               |
| [4]     | WREN      | DFI write data enable bus                |
| [5]     | RDEN      | DFI read data enable bus                 |
| [6]     | CS_TOP    | DFI chip select bus for top half only    |
| [7]     | CS_BOTTOM | DFI chip select bus for bottom half only |

### SDRAM_DFII_PI1_COMMAND_ISSUE

Address: 0x00001000 + 0x20 = 0x00001020

### SDRAM_DFII_PI1_ADDRESS

Address: 0x00001000 + 0x24 = 0x00001024

> DFI address bus

### SDRAM_DFII_PI1_BADDRESS

Address: 0x00001000 + 0x28 = 0x00001028

> DFI bank address bus

### SDRAM_DFII_PI1_WRDATA

Address: 0x00001000 + 0x2c = 0x0000102c

> DFI write data bus

### SDRAM_DFII_PI1_RDDATA

Address: 0x00001000 + 0x30 = 0x00001030

> DFI read data bus

### SDRAM_DFII_PI2_COMMAND

Address: 0x00001000 + 0x34 = 0x00001034

> Control DFI signals on a single phase

| Field   | Name      | Description                              |
|---------|-----------|------------------------------------------|
| [0]     | CS        | DFI chip select bus                      |
| [1]     | WE        | DFI write enable bus                     |
| [2]     | CAS       | DFI column address strobe bus            |
| [3]     | RAS       | DFI row address strobe bus               |
| [4]     | WREN      | DFI write data enable bus                |
| [5]     | RDEN      | DFI read data enable bus                 |
| [6]     | CS_TOP    | DFI chip select bus for top half only    |
| [7]     | CS_BOTTOM | DFI chip select bus for bottom half only |

### SDRAM_DFII_PI2_COMMAND_ISSUE

Address: 0x00001000 + 0x38 = 0x00001038

### SDRAM_DFII_PI2_ADDRESS

Address: 0x00001000 + 0x3c = 0x0000103c

> DFI address bus

### SDRAM_DFII_PI2_BADDRESS

Address: 0x00001000 + 0x40 = 0x00001040

> DFI bank address bus

### SDRAM_DFII_PI2_WRDATA

Address: 0x00001000 + 0x44 = 0x00001044

> DFI write data bus

### SDRAM_DFII_PI2_RDDATA

Address: 0x00001000 + 0x48 = 0x00001048

> DFI read data bus

### SDRAM_DFII_PI3_COMMAND

Address: 0x00001000 + 0x4c = 0x0000104c

> Control DFI signals on a single phase

| Field   | Name      | Description                              |
|---------|-----------|------------------------------------------|
| [0]     | CS        | DFI chip select bus                      |
| [1]     | WE        | DFI write enable bus                     |
| [2]     | CAS       | DFI column address strobe bus            |
| [3]     | RAS       | DFI row address strobe bus               |
| [4]     | WREN      | DFI write data enable bus                |
| [5]     | RDEN      | DFI read data enable bus                 |
| [6]     | CS_TOP    | DFI chip select bus for top half only    |
| [7]     | CS_BOTTOM | DFI chip select bus for bottom half only |

### SDRAM_DFII_PI3_COMMAND_ISSUE

Address: 0x00001000 + 0x50 = 0x00001050

### SDRAM_DFII_PI3_ADDRESS

Address: 0x00001000 + 0x54 = 0x00001054

> DFI address bus

### SDRAM_DFII_PI3_BADDRESS

Address: 0x00001000 + 0x58 = 0x00001058

> DFI bank address bus

### SDRAM_DFII_PI3_WRDATA

Address: 0x00001000 + 0x5c = 0x0000105c

> DFI write data bus

### SDRAM_DFII_PI3_RDDATA

Address: 0x00001000 + 0x60 = 0x00001060

> DFI read data bus
