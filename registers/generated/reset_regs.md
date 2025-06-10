# Reset Register map

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

Base address: 0x100000d0

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [CTRL](#ctrl)            | 0x00000000 | Reset control register |
| [REASON](#reason)        | 0x00000004 | Reset reason register |

## CTRL

Reset control register

Address offset: 0x00000000

Reset value: 0x00000000

![ctrl](md_img/ctrl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:3   | -               | 0x0000000  | Reserved |
| USB_RESET        | 2      | wo              | 0x0        | USB module reset |
| DM_RESET         | 1      | wo              | 0x0        | Debug module reset |
| NDM_RESET        | 0      | wo              | 0x0        | Non-debug module reset |

Back to [Register map](#register-map-summary).

## REASON

Reset reason register

Address offset: 0x00000004

Reset value: 0x00000000

![reason](md_img/reason.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:6   | -               | 0x000000   | Reserved |
| SW_USB           | 5      | roc             | 0x0        | Software triggered USB reset |
| EXT              | 4      | roc             | 0x0        | External reset |
| NDM              | 3      | roc             | 0x0        | Non-debug module reset |
| SW_DM            | 2      | roc             | 0x0        | Software triggered DM reset |
| SW_NDM           | 1      | roc             | 0x0        | Software triggered NDM reset |
| POR              | 0      | roc             | 0x0        | Power-on reset |

Back to [Register map](#register-map-summary).
