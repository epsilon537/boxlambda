# DFX Controller Control Register

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
| [CONTROL](#control)      | 0x00000000 | Control register. |

## CONTROL

Control register.

Address offset: 0x00000000

Reset value: 0x00000000

![control](md_img/control.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| HALFWORD         | 31:16  | wo              | 0x0000     | Halfword field containing extra info. |
| BYTE             | 15:8   | rw              | 0x00       | Byte field containing extra info. |
| CMD              | 7:0    | rw              | 0x00       | Command. |

Enumerated values for CONTROL.CMD.

| Name             | Value   | Description |
| :---             | :---    | :---        |
| SHUTDOWN         | 0x00   | Shutdown. |
| RESTART_NO_STAT  | 0x01   | Restart without status. |
| RESTART_STAT     | 0x02   | Restart with status. |
| PROCEED          | 0x03   | Proceed. |
| USR_CTRL         | 0x04   | Proceed. |

Back to [Register map](#register-map-summary).
