# The Base Platform Architecture

![Base Platform Architecture](../../assets/base-platform-arch.png)

*BoxLambda Base Platform Architecture.*

The BoxLambda Base Platform consists of:

- a collection of reusable target software components, including:
  - HAL/driver-level components interacting with specific features of the BoxLambda SoC (USB HID, SDSPI driver, ...).
  - Higher level software components such as FATFS, ST-Sound player...
- a bootstrap system creating a target C environment.
- a build system allowing the user to easily add components and applications using those components.
  - The **Bootloader** is one such application.
  - The main application is the [BoxLambda OS](../applications/boxlambda-os/architecture.md).
  - Other applications are test programs supporting test gateware builds such as the [DDR test](../../soc/test/builds/ddr.md), [USB-HID test](../../soc/test/builds/usb-hid.md).

## Index

- Bootstrap:
  - [The Bootstrap Component](bootstrap/bootstrap-component.md)
  - [Bootloader](bootstrap/bootloader.md)
  - [The Linker Script](bootstrap/linker-script.md)
- C Components:
  - [About C Components](c-components/about.md)
  - [Register Access Layer](c-components/register-access-layer.md)
  - [LiteDRAM Initialization](c-components/litedram-initialization.md)
  - [SDSPI](c-components/sdspi.md)
  - [FatFs](c-components/fat-fs.md)
  - [ST-Sound](c-components/st-sound.md)
  - [VERA HAL](c-components/vera-hal.md)
  - [USB HID](c-components/usb-hid.md)
  - [Flash Driver](c-components/flashdrvr.md)
  - [UART](c-components/uart.md)
  - [GPIO](c-components/gpio.md)
  - [RISC-V](c-components/riscv.md)
  - [I2C](c-components/i2c.md)
  - [Real-Time Clock and Calendar](c-components/rtcc.md)
  - [DFX Controller HAL](c-components/dfx-controller-hal.md)
  - [Common](c-components/common.md)
- Test C Components:
  - [About Test C Components](c-components/test/about.md)
  - [Timer](c-components/test/timer.md)
  - [Interrupt Handling](c-components/test/irqs.md)
  - [Embedded CLI](c-components/test/cli.md)
  - [Ymodem](c-components/test/ymodem.md)
  - [Memory and File System CLI](c-components/test/mem-fs-cli.md)
  - [Peek and Poke CLI](c-components/test/peek-poke-cli.md)
  - [J1B HAL](c-components/test/j1b-hal.md)
  - [VS0 HAL](c-components/test/vs0-hal.md)
- The Software Build System:
  - [The Build System](build-sys/build-sys.md)
  - [Building the Software](build-sys/building.md)



