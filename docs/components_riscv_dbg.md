## RISCV-DBG Debug Core

- **RISCV-DBG Repo**, BoxLambda fork, *boxlambda* branch: 
  [https://github.com/epsilon537/riscv-dbg](https://github.com/epsilon537/riscv-dbg).

- **RISV-DBG Submodule in the BoxLambda Directory Tree**: 
  boxlambda/sub/riscv-dbg/.

- **RISCV-DBG Gateware Component in the BoxLambda Directory Tree**: 
  [boxlambda/gw/components/riscv-dbg](https://github.com/epsilon537/boxlambda/tree/master/gw/components/riscv-dbg)

### RISCV OpenOCD

OpenOCD is an open-source software package used to interface with a hardware debugger's JTAG port via one of many transport protocols. In our case, the hardware debug logic is implemented by a component called **riscv-dbg**. The overall setup looks like this:

![OpenOCD General Setup](assets/OpenOCD_Setup_General.drawio.png)

*OpenOCD General Setup*

BoxLambda uses the RISCV fork of OpenOCD: [https://github.com/riscv/riscv-openocd](https://github.com/riscv/riscv-openocd)

### The RISCV-DBG component

RISCV-dbg is part of the [*PULP* platform](https://github.com/pulp-platform) and depends on three additional GitHub repositories that are part of this platform:

- **common_cells**: [https://github.com/pulp-platform/common_cells](https://github.com/pulp-platform/common_cells)
- **tech_cells_generic**: [https://github.com/pulp-platform/tech_cells_generic](https://github.com/pulp-platform/tech_cells_generic)
- **pulpino**: [https://github.com/pulp-platform/pulpino](https://github.com/pulp-platform/pulpino)

As their names suggest, *common_cells* and *tech_cells_generic* provide commonly used building blocks such as FIFOs, CDC logic, reset logic, etc. *Pulpino* is an entire RISCV-based SoC project. However, the riscv-dbg pulpino dependency is limited to just a few cells for clock management.

I created git submodules for all of these repositories under the BoxLambda repository's *sub/* directory. I also created a riscv-dbg component directory with a Bender.yml manifest in it, referencing all the sources needed from those submodules: [gw/components/riscv-dbg/Bender.yml](https://github.com/epsilon537/boxlambda/blob/master/gw/components/riscv-dbg/Bender.yml).

```
boxlambda
├── gw
│   └── components
│       └── riscv-dbg
│           └── Bender.yml
└── sub
    ├── common_cells
    ├── tech_cells_generic
    ├── pulpino	
    └── riscv-dbg

```

### RISCV-DBG RTL Structure

RISCV-DBG has two top-levels:

- [sub/riscv-dbg/src/dm_top.sv](https://github.com/epsilon537/riscv-dbg/blob/b241f967f0dd105f7c5e020a395bbe0ec54e40e4/src/dm_top.sv)
- [sub/riscv-dbg/src/dmi_jtag.sv](https://github.com/epsilon537/riscv-dbg/blob/b241f967f0dd105f7c5e020a395bbe0ec54e40e4/src/dmi_jtag.sv)

Recall that BoxLambda uses a Wishbone interconnect. The Ibex_WB submodule implements a Wishbone wrapper for the Ibex RISCV core. It does the same for RISCV-DBG's *dm_top*:  
[sub/ibex_wb/rtl/wb_dm_top.sv](https://github.com/epsilon537/ibex_wb/blob/87a97e38f3cf15bee80eb69bfa82166c00842b1e/rtl/wb_dm_top.sv)

Refer to the *boxlambda_soc.sv* module to see how RISCV-DBG is instantiated:
[gw/components/boxlambda_soc/rtl/boxlambda_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/boxlambda_soc/rtl/boxlambda_soc.sv)  

### OpenOCD and RISCV-DBG on Verilator

The JTAG transport protocol used on the Verilator Model is a simple socket-based protocol called **Remote Bitbang**.
The remote bitbang spec is just one page: 

[https://github.com/openocd-org/openocd/blob/master/doc/manual/jtag/drivers/remote_bitbang.txt](https://github.com/openocd-org/openocd/blob/master/doc/manual/jtag/drivers/remote_bitbang.txt)

The Verilator setup looks like this:

![BoxLambda OpenOCD Verilator Setup](assets/OpenOCD_Setup_Verilator.drawio.png)

*BoxLambda OpenOCD Verilator Setup*

The **hello_dbg** project (directory *gw/projects/hello_dbg/*) implements the OpenOCD Verilator setup shown above. The project contains the Hello World test build extended with the riscv-dbg component.
The project directory also contains a [test script](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/hello_dbg/test/test.sh) that goes through the following steps:

1. Start the Verilator model
2. Connect OpenOCD to the model
3. Connect GDB to OpenOCD (and thus to the model)
4. Execute a UART register dump on the target
5. Check the UART register contents against expected results. 

```
boxlambda
├── gw
│   ├── projects
│   │   └── hello-dbg
│   │       ├── Bender.yml
│   │       ├── sim
│   │       │   ├── sim_main.cpp
│   │       │   └── sim_main.sv
│   │       └── test
│   │           ├── test.sh
│   │           └── test.gdb 
│   └── components
│       └── riscv-dbg
└── sub
    ├── common_cells
    ├── tech_cells_generic
    ├── pulpino	
    └── riscv-dbg

```

The OpenOCD configuration file for JTAG Debugging on Verilator is checked into the *scripts/* directory: [scripts/verilator_riscv_dbg.cfg](https://github.com/epsilon537/boxlambda/blob/master/scripts/verilator_riscv_dbg.openocd.cfg)

To summarize:

1. The above OpenOCD config file is used to connect to the JTAG TAP of a Verilator model.
2. The JTAG TAP is implemented by a riscv-dbg core connected to an Ibex RISCV32 core.
2. The JTAG TAP is used to debug the software running on the Ibex RISCV32 core.
3. The JTAG TAP is accessed using a socket-based OpenOCD transport protocol called **remote_bitbang**.

See the **Test Builds** section for the steps needed to set up an OpenOCD JTAG debug session on Verilator.

### OpenOCD and RISCV-DBG on Arty-A7 FPGA

The obvious approach would be to bring out the JTAG signals to PMOD pins and hook up a JTAG adapter. However, there's an alternative method that doesn't require a JTAG adapter. The riscv-dbg JTAG TAP can be hooked into the FPGA scan chain which is normally used to program the bitstream into the FPGA. On the Arty-A7, bitstream programming is done using the FTDI USB serial port, so no special adapters are needed.

The riscv-dbg codebase lets you easily switch between a variant with external JTAG pins and a variant that hooks into the FPGA scan chain, by changing a single file:

- [dmi_jtag_tap.sv](https://github.com/epsilon537/riscv-dbg/blob/boxlambda/src/dmi_jtag_tap.sv): hooks up the JTAG TAP to external pins
- [dmi_bscane_tap.sv](https://github.com/epsilon537/riscv-dbg/blob/boxlambda/src/dmi_bscane_tap.sv): hooks the JTAG TAP into the FPGA scan chain. The Xilinx primitive used to hook into the scan chain do this is called BSCANE. Hence the name.

Both files implement the same module name (*dmi_jtag_tap*) and the same module ports, so you can swap one for the other without further impact on the system. Lightweight polymorphism. It sounds cool, but it feels a bit forced. One annoying consequence is that although the JTAG ports are listed at the top-level, they are not used when building the BSCANE variant. The BSCANE primitive internally provides the actual JTAG ports. Anyway, that's the RISCV-DBG design. I'll go with it. By default, BoxLambda builds the BSCANE variant on the Arty-A7.

On the OpenOCD side, the transport protocol for this Debug-Access-via-FPGA-scan-chain-over-FTDI is anti-climactically called **ftdi**.

![BoxLambda OpenOCD Arty A7 FTDI Setup](assets/OpenOCD_Setup_Arty_A7.drawio.png)

*BoxLambda OpenOCD Arty A7 FTDI Setup*

The OpenOCD configuration file for JTAG Debugging on Arty A7 is checked into the *scripts/* directory:

- Arty-A7-35T: [scripts/digilent_arty_a7_35.cfg](https://github.com/epsilon537/boxlambda/blob/master/scripts/digilent_arty_a7_35.openocd.cfg) 
- Art-A7-100T: [scripts/digilent_arty_a7_100.cfg](https://github.com/epsilon537/boxlambda/blob/master/scripts/digilent_arty_a7_100.openocd.cfg) 

To summarize:

1. The above OpenOCD config file is used to connect to the JTAG TAP of a riscv-dbg core...
2. ...to debug the software running on a connected Ibex RISCV32 core.
3. The riscv-dbg core's JTAG TAP is hooked into the Arty-A7's scan chain, normally used for loading a bitstream into the FPGA.
4. The Arty-A7 FPGA scan chain is accessible through the board's FTDI-based USB serial port.
5. The OpenOCD transport protocol name for this type of connection is **ftdi**.

See the **Test Builds** section for the steps needed to set up an OpenOCD JTAG debug session on the Arty A7.

### RISCV-DBG Clock Frequency

*Dm_top* and *dmi_jtag* are part of the 50MHz System Clock domain.

The JTAG clock *tck* is driven via a *BSCANE2* primitive by the FPGA's JTAG chain. The BSCANE2 primitive is instantiated in the [dmi_bscane_tap](https://github.com/epsilon537/riscv-dbg/blob/boxlambda/src/dmi_bscane_tap.sv) module.
