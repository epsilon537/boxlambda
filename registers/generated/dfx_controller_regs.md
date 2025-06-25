# DFX Controller Register map

Created with [Corsair](https://github.com/esynr3z/corsair) v1.0.4.

## Conventions

| Access mode | Description               |
| :---------- | :------------------------ |
| rw          | Read and Write            |
| rw1c        | Read and Write 1 to Clear |
| rw1s        | Read and Write 1 to Set   |
| ro          | Read Only                 |
| roc         | Read Only to Clear        |
| roll        | Read Only / Latch Low     |
| rolh        | Read Only / Latch High    |
| wo          | Write only                |
| wosc        | Write Only / Self Clear   |

## Register map summary

Base address: 0x10000400

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [STATUS](#status)        | 0x00000000 | Status register. |
| [SW_TRIGGER](#sw_trigger) | 0x00000004 | Software trigger register. |
| [TRIGGER_0](#trigger_0)  | 0x00000020 | Trigger 0. |
| [TRIGGER_1](#trigger_1)  | 0x00000024 | Trigger 1. |
| [RM_BS_INDEX_0](#rm_bs_index_0) | 0x00000040 | Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0. |
| [RM_CONTROL_0](#rm_control_0) | 0x00000044 | Control info for Reconfigurable Module 0. |
| [RM_BS_INDEX_1](#rm_bs_index_1) | 0x00000048 | Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1. |
| [RM_CONTROL_1](#rm_control_1) | 0x0000004c | Control info for Reconfigurable Module 1. |
| [BS_ID_0](#bs_id_0)      | 0x00000060 | Bitstream 0 ID. |
| [BS_ADDRESS_0](#bs_address_0) | 0x00000064 | Bitstream 0 byte address. |
| [BS_SIZE_0](#bs_size_0)  | 0x00000068 | Bitstream 0 size in bytes. |
| [BS_ID_1](#bs_id_1)      | 0x00000070 | Bitstream 1 ID. |
| [BS_ADDRESS_1](#bs_address_1) | 0x00000074 | Bitstream 1 byte address. |
| [BS_SIZE_1](#bs_size_1)  | 0x00000078 | Bitstream 1 size in bytes. |

## STATUS

Status register.

Address offset: 0x00000000

Reset value: 0x00000000

![status](md_img/status.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:24  | -               | 0x00       | Reserved |
| RM_ID            | 23:8   | ro              | 0x0000     | ID of Reconfigurable module to which the status applies. |
| SHUTDOWN         | 7      | ro              | 0x0        | Set if controller is int shutdown state. |
| ERR              | 6:3    | ro              | 0x0        | Error state. |
| STATE            | 2:0    | ro              | 0x0        | Current state. |

Enumerated values for STATUS.STATE.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| VS_EMPTY         | 0x0    | Virtual socket empty. |
| HW_SHUTDOWN      | 0x1    | Hardware shutdown step. |
| SW_SHUTDOWN      | 0x2    | Executing software shutdown step. |
| LOADING          | 0x4    | Loading the new reconfigurable module. |
| SW_STARTUP       | 0x5    | Executing software startup step. |
| RM_RESET         | 0x6    | Executing reconfigurable module reset step. |
| VS_FULL          | 0x7    | Virtual Socket is full. |

Enumerated values for STATUS.ERR.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| NO_ERR           | 0x0    | No error. |
| BAD_CONFIG       | 0x1    | Bad configuration error. |
| BS               | 0x2    | Bitstream error. |
| FETCH            | 0x4    | Fetch error. |
| BS_FETCH         | 0x5    | Bitstream and fetch error. |
| BAD_SIZE         | 0x7    | Bad size error. |
| BAD_FORMAT       | 0x8    | Bad format error. |
| UNKNOWN          | 0xf    | Unknown error. |

Back to [Register map](#register-map-summary).

## SW_TRIGGER

Software trigger register.

Address offset: 0x00000004

Reset value: 0x00000000

![sw_trigger](md_img/sw_trigger.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| TRIGGER_PENDING  | 31     | ro              | 0x0        | Software trigger pending. |
| -                | 30:1   | -               | 0x0000000  | Reserved |
| TRIGGER_ID       | 0      | rw              | 0x0        | Trigger ID. |

Back to [Register map](#register-map-summary).

## TRIGGER_0

Trigger 0.

Address offset: 0x00000020

Reset value: 0x00000000

![trigger_0](md_img/trigger_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | ID of Reconfigurable Module to load if trigger 0 is asserted. |

Back to [Register map](#register-map-summary).

## TRIGGER_1

Trigger 1.

Address offset: 0x00000024

Reset value: 0x00000000

![trigger_1](md_img/trigger_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | ID of Reconfigurable Module to load if trigger 1 is asserted. |

Back to [Register map](#register-map-summary).

## RM_BS_INDEX_0

Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.

Address offset: 0x00000040

Reset value: 0x00000000

![rm_bs_index_0](md_img/rm_bs_index_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:16  | -               | 0x0000     | Reserved |
| INDEX            | 15:0   | rw              | 0x0000     | Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0. |

Back to [Register map](#register-map-summary).

## RM_CONTROL_0

Control info for Reconfigurable Module 0.

Address offset: 0x00000044

Reset value: 0x00000000

![rm_control_0](md_img/rm_control_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:13  | -               | 0x0000     | Reserved |
| RST_DURATION     | 12:5   | rw              | 0x00       | Reset duration in clock cycles. |
| RST_REQUIRED     | 4:3    | rw              | 0x0        | Reset required. |
| STARTUP_REQUIRED | 2      | rw              | 0x0        | Software startup required. |
| SHUTDOWN_REQUIRED | 1:0    | rw              | 0x0        | Shutdown required. |

Enumerated values for RM_CONTROL_0.SHUTDOWN_REQUIRED.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| NO_SHUTDOWN      | 0x0    | No shutdown required. |
| HW_RM            | 0x1    | Hardware Reconfigurable Module shutdown required. |
| HW_THEN_SW       | 0x2    | Hardware then software shutdown required. |
| SW_THEN_HW       | 0x3    | Software then hardware shutdown required. |

Enumerated values for RM_CONTROL_0.RST_REQUIRED.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| NO_RST           | 0x0    | No reset required. |
| ACTIVE_LO_RST    | 0x2    | Active low reset required. |
| ACTIVE_HI_RST    | 0x3    | Active high reset required. |

Back to [Register map](#register-map-summary).

## RM_BS_INDEX_1

Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.

Address offset: 0x00000048

Reset value: 0x00000000

![rm_bs_index_1](md_img/rm_bs_index_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:16  | -               | 0x0000     | Reserved |
| INDEX            | 15:0   | rw              | 0x0000     | Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1. |

Back to [Register map](#register-map-summary).

## RM_CONTROL_1

Control info for Reconfigurable Module 1.

Address offset: 0x0000004c

Reset value: 0x00000000

![rm_control_1](md_img/rm_control_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:13  | -               | 0x0000     | Reserved |
| RST_DURATION     | 12:5   | rw              | 0x00       | Reset duration in clock cycles. |
| RST_REQUIRED     | 4:3    | rw              | 0x0        | Reset required. |
| STARTUP_REQUIRED | 2      | rw              | 0x0        | Software startup required. |
| SHUTDOWN_REQUIRED | 1:0    | rw              | 0x0        | Shutdown required. |

Enumerated values for RM_CONTROL_1.SHUTDOWN_REQUIRED.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| NO_SHUTDOWN      | 0x0    | No shutdown required. |
| HW_RM            | 0x1    | Hardware Reconfigurable Module shutdown required. |
| HW_THEN_SW       | 0x2    | Hardware then software shutdown required. |
| SW_THEN_HW       | 0x3    | Software then hardware shutdown required. |

Enumerated values for RM_CONTROL_1.RST_REQUIRED.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| NO_RST           | 0x0    | No reset required. |
| ACTIVE_LO_RST    | 0x2    | Active low reset required. |
| ACTIVE_HI_RST    | 0x3    | Active high reset required. |

Back to [Register map](#register-map-summary).

## BS_ID_0

Bitstream 0 ID.

Address offset: 0x00000060

Reset value: 0x00000000

![bs_id_0](md_img/bs_id_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Bitstream 0 ID value. |

Back to [Register map](#register-map-summary).

## BS_ADDRESS_0

Bitstream 0 byte address.

Address offset: 0x00000064

Reset value: 0x00000000

![bs_address_0](md_img/bs_address_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Bitstream 0 byte address. |

Back to [Register map](#register-map-summary).

## BS_SIZE_0

Bitstream 0 size in bytes.

Address offset: 0x00000068

Reset value: 0x00000000

![bs_size_0](md_img/bs_size_0.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Bitstream 0 size in bytes. |

Back to [Register map](#register-map-summary).

## BS_ID_1

Bitstream 1 ID.

Address offset: 0x00000070

Reset value: 0x00000000

![bs_id_1](md_img/bs_id_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Bitstream 1 ID value. |

Back to [Register map](#register-map-summary).

## BS_ADDRESS_1

Bitstream 1 byte address.

Address offset: 0x00000074

Reset value: 0x00000000

![bs_address_1](md_img/bs_address_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Bitstream 1 byte address value. |

Back to [Register map](#register-map-summary).

## BS_SIZE_1

Bitstream 1 size in bytes.

Address offset: 0x00000078

Reset value: 0x00000000

![bs_size_1](md_img/bs_size_1.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| VALUE            | 31:0   | rw              | 0x00000000 | Bitstream 1 size in bytes value. |

Back to [Register map](#register-map-summary).
