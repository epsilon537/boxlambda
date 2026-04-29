# SDRAM

## Register Listing for SDRAM

| Register                                                      | Address                                     |
|---------------------------------------------------------------|---------------------------------------------|
| [SDRAM_DFII_CONTROL](#sdram-dfii-control)                     | [0x00000800](#sdram-dfii-control)           |
| [SDRAM_DFII_PI0_COMMAND](#sdram-dfii-pi0-command)             | [0x00000804](#sdram-dfii-pi0-command)       |
| [SDRAM_DFII_PI0_COMMAND_ISSUE](#sdram-dfii-pi0-command-issue) | [0x00000808](#sdram-dfii-pi0-command-issue) |
| [SDRAM_DFII_PI0_ADDRESS](#sdram-dfii-pi0-address)             | [0x0000080c](#sdram-dfii-pi0-address)       |
| [SDRAM_DFII_PI0_BADDRESS](#sdram-dfii-pi0-baddress)           | [0x00000810](#sdram-dfii-pi0-baddress)      |
| [SDRAM_DFII_PI0_WRDATA](#sdram-dfii-pi0-wrdata)               | [0x00000814](#sdram-dfii-pi0-wrdata)        |
| [SDRAM_DFII_PI0_RDDATA](#sdram-dfii-pi0-rddata)               | [0x00000818](#sdram-dfii-pi0-rddata)        |
| [SDRAM_DFII_PI1_COMMAND](#sdram-dfii-pi1-command)             | [0x0000081c](#sdram-dfii-pi1-command)       |
| [SDRAM_DFII_PI1_COMMAND_ISSUE](#sdram-dfii-pi1-command-issue) | [0x00000820](#sdram-dfii-pi1-command-issue) |
| [SDRAM_DFII_PI1_ADDRESS](#sdram-dfii-pi1-address)             | [0x00000824](#sdram-dfii-pi1-address)       |
| [SDRAM_DFII_PI1_BADDRESS](#sdram-dfii-pi1-baddress)           | [0x00000828](#sdram-dfii-pi1-baddress)      |
| [SDRAM_DFII_PI1_WRDATA](#sdram-dfii-pi1-wrdata)               | [0x0000082c](#sdram-dfii-pi1-wrdata)        |
| [SDRAM_DFII_PI1_RDDATA](#sdram-dfii-pi1-rddata)               | [0x00000830](#sdram-dfii-pi1-rddata)        |
| [SDRAM_DFII_PI2_COMMAND](#sdram-dfii-pi2-command)             | [0x00000834](#sdram-dfii-pi2-command)       |
| [SDRAM_DFII_PI2_COMMAND_ISSUE](#sdram-dfii-pi2-command-issue) | [0x00000838](#sdram-dfii-pi2-command-issue) |
| [SDRAM_DFII_PI2_ADDRESS](#sdram-dfii-pi2-address)             | [0x0000083c](#sdram-dfii-pi2-address)       |
| [SDRAM_DFII_PI2_BADDRESS](#sdram-dfii-pi2-baddress)           | [0x00000840](#sdram-dfii-pi2-baddress)      |
| [SDRAM_DFII_PI2_WRDATA](#sdram-dfii-pi2-wrdata)               | [0x00000844](#sdram-dfii-pi2-wrdata)        |
| [SDRAM_DFII_PI2_RDDATA](#sdram-dfii-pi2-rddata)               | [0x00000848](#sdram-dfii-pi2-rddata)        |
| [SDRAM_DFII_PI3_COMMAND](#sdram-dfii-pi3-command)             | [0x0000084c](#sdram-dfii-pi3-command)       |
| [SDRAM_DFII_PI3_COMMAND_ISSUE](#sdram-dfii-pi3-command-issue) | [0x00000850](#sdram-dfii-pi3-command-issue) |
| [SDRAM_DFII_PI3_ADDRESS](#sdram-dfii-pi3-address)             | [0x00000854](#sdram-dfii-pi3-address)       |
| [SDRAM_DFII_PI3_BADDRESS](#sdram-dfii-pi3-baddress)           | [0x00000858](#sdram-dfii-pi3-baddress)      |
| [SDRAM_DFII_PI3_WRDATA](#sdram-dfii-pi3-wrdata)               | [0x0000085c](#sdram-dfii-pi3-wrdata)        |
| [SDRAM_DFII_PI3_RDDATA](#sdram-dfii-pi3-rddata)               | [0x00000860](#sdram-dfii-pi3-rddata)        |

### SDRAM_DFII_CONTROL

Address: 0x00000800 + 0x0 = 0x00000800

> Control DFI signals common to all phases

| Field   | Name    | Description                                                                                                                                                                         |
|---------|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [0]     | SEL     | | Value   | Description                 |<br/>|---------|-----------------------------|<br/>| `0b0`   | Software (CPU) control.     |<br/>| `0b1`   | Hardware control (default). | |
| [1]     | CKE     | DFI clock enable bus                                                                                                                                                                |
| [2]     | ODT     | DFI on-die termination bus                                                                                                                                                          |
| [3]     | RESET_N | DFI clock reset bus                                                                                                                                                                 |

### SDRAM_DFII_PI0_COMMAND

Address: 0x00000800 + 0x4 = 0x00000804

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

Address: 0x00000800 + 0x8 = 0x00000808

### SDRAM_DFII_PI0_ADDRESS

Address: 0x00000800 + 0xc = 0x0000080c

> DFI address bus

### SDRAM_DFII_PI0_BADDRESS

Address: 0x00000800 + 0x10 = 0x00000810

> DFI bank address bus

### SDRAM_DFII_PI0_WRDATA

Address: 0x00000800 + 0x14 = 0x00000814

> DFI write data bus

### SDRAM_DFII_PI0_RDDATA

Address: 0x00000800 + 0x18 = 0x00000818

> DFI read data bus

### SDRAM_DFII_PI1_COMMAND

Address: 0x00000800 + 0x1c = 0x0000081c

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

Address: 0x00000800 + 0x20 = 0x00000820

### SDRAM_DFII_PI1_ADDRESS

Address: 0x00000800 + 0x24 = 0x00000824

> DFI address bus

### SDRAM_DFII_PI1_BADDRESS

Address: 0x00000800 + 0x28 = 0x00000828

> DFI bank address bus

### SDRAM_DFII_PI1_WRDATA

Address: 0x00000800 + 0x2c = 0x0000082c

> DFI write data bus

### SDRAM_DFII_PI1_RDDATA

Address: 0x00000800 + 0x30 = 0x00000830

> DFI read data bus

### SDRAM_DFII_PI2_COMMAND

Address: 0x00000800 + 0x34 = 0x00000834

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

Address: 0x00000800 + 0x38 = 0x00000838

### SDRAM_DFII_PI2_ADDRESS

Address: 0x00000800 + 0x3c = 0x0000083c

> DFI address bus

### SDRAM_DFII_PI2_BADDRESS

Address: 0x00000800 + 0x40 = 0x00000840

> DFI bank address bus

### SDRAM_DFII_PI2_WRDATA

Address: 0x00000800 + 0x44 = 0x00000844

> DFI write data bus

### SDRAM_DFII_PI2_RDDATA

Address: 0x00000800 + 0x48 = 0x00000848

> DFI read data bus

### SDRAM_DFII_PI3_COMMAND

Address: 0x00000800 + 0x4c = 0x0000084c

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

Address: 0x00000800 + 0x50 = 0x00000850

### SDRAM_DFII_PI3_ADDRESS

Address: 0x00000800 + 0x54 = 0x00000854

> DFI address bus

### SDRAM_DFII_PI3_BADDRESS

Address: 0x00000800 + 0x58 = 0x00000858

> DFI bank address bus

### SDRAM_DFII_PI3_WRDATA

Address: 0x00000800 + 0x5c = 0x0000085c

> DFI write data bus

### SDRAM_DFII_PI3_RDDATA

Address: 0x00000800 + 0x60 = 0x00000860

> DFI read data bus
