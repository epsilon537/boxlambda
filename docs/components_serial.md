---
hide:
  - toc
---

## Serial Port

- **Wbuart32 Repo**, BoxLambda fork, `boxlambda` branch:
    [https://github.com/epsilon537/wbuart32](https://github.com/epsilon537/wbuart32)

- **Wbuart32 Submodule in the BoxLambda Directory Tree**:
    boxlambda/sub/wbuart32/

- **Wbuart Component in the BoxLambda Directory Tree**:
    [boxlambda/gw/components/wbuart](https://github.com/epsilon537/boxlambda/tree/master/gw/components/wbuart)

- **Wbuart Top-Level**:
    [sub/wbuart32/rtl/wbuart.v](https://github.com/epsilon537/wbuart32/blob/boxlambda/rtl/wbuart.v)

- **Wbuart Spec:**:
    [sub/wbuart32/doc/spec.pdf](https://github.com/epsilon537/wbuart32/blob/boxlambda/doc/spec.pdf)

ZipCPU comes to the rescue again with a UART implementation with a Wishbone interface. The BoxLambda variant of the wbuart core contains modifications for enabling and clearing specific interrupt events. These modifications are documented in a separate section of the core's [spec](https://github.com/epsilon537/wbuart32/blob/boxlambda/doc/spec.pdf).

The wbuart core is instantiated *without* hardware flow control.

### Wbuart Clock Frequency

The Wbuart core is part of the 50MHz System Clock Domain.

