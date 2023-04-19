Components
==========

We're building a System-on-a-Chip (*System-on-an-FPGA*?). This section identifies the Key Components of the BoxLambda SoC.

### The Wishbone Interconnect

The Bus, or Interconnect, is the fabric stitching together the SoC internal components. For this project, the two most relevant SoC internal bus specifications are [ARM's AXI bus](https://developer.arm.com/documentation/ihi0022/latest) and the Open-Source [Wishbone bus](https://wishbone-interconnect.readthedocs.io/en/latest/).

**AXI** is very powerful, very popular, and very complex. It scales up well to very big SoCs. However, I don't think it scales down very well to simple SoCs, such as BoxLambda, where low latency and low complexity are more important than high bandwidth and scalability. Hence, for this project, I'm electing to go with **Wishbone**. 

We'll be using Wishbone in *Pipelined Mode*, as specified in the [Wishbone B4 specification](https://github.com/fossi-foundation/wishbone/blob/master/documents/spec/wbspec_b4.pdf).

#### Bus Width and Clock Frequency

We'll be using a 32-bit data bus. 

The Bus Fabric is part of the 50MHz System Clock Domain.

### RISCV Ibex Processor

There are a lot of RISC-V implementations to choose from. The **Ibex** project seems like a good choice:

[https://github.com/lowRISC/ibex](https://github.com/lowRISC/ibex)

- 32-bit RISC-V.
- High-quality, well-documented implementation.
- SystemVerilog based.
- Supports a *small* two-stage pipeline parameterization.
- Very active project.

Location of the *Ibex* submodule in the BoxLambda repo: **sub/ibex/**

The Ibex core:
[sub/ibex/rtl/ibex_top.sv](https://github.com/epsilon537/ibex/blob/acdf41b2bf3ed2f33ed5c29e65c1625d22e4aab5/rtl/ibex_top.sv)

#### Ibex Wishbone Wrapper: *Ibex_WB*

[https://github.com/epsilon537/ibex_wb](https://github.com/epsilon537/ibex_wb) forked from [https://github.com/batuhanates/ibex_wb](https://github.com/batuhanates/ibex_wb)

The Ibex RISCV core itself doesn't have Wishbone ports. *Ibex_WB* wraps around the vanilla Ibex core and attaches Wishbone port adapters to its instruction and data ports.

The *Ibex_WB* repo also includes an example SoC build consisting of an Ibex core connected via a shared Wishbone bus to a wbuart32 core and an internal memory module, along with the software to run on that platform. This example SoC is the starting point for BoxLambda's implementation. See the [Test Builds section](installation-and-test-builds.md#test-builds).

Location of the *Ibex_WB* submodule in the BoxLambda repo: **sub/ibex/**

The *wb_ibex_core*:
[sub/ibex_wb/rtl/wb_ibex_core.sv](https://github.com/epsilon537/ibex_wb/blob/87a97e38f3cf15bee80eb69bfa82166c00842b1e/rtl/wb_ibex_core.sv)

###### From Ibex to LiteDRAM

This is the Ibex Memory Interface specification:

[https://ibex-core.readthedocs.io/en/latest/03_reference/load_store_unit.html#load-store-unit](https://ibex-core.readthedocs.io/en/latest/03_reference/load_store_unit.html#load-store-unit)

There are two such interfaces. One for data, one for instructions.

The job of *core2wb* is to adapt that interface to a pipelined Wishbone bus master interface. That Wishbone bus master in turn requests access to the shared bus. It's up to *wb_interconnect_shared_bus* to grant the bus to one of the requesting bus masters and direct the transaction to the selected slave. If either one of those modules has a bug, that will result in an incorrectly behaving bus master, from the point of view of the bus slave.

![Ibex to LiteDRAM.](assets/Ibex_to_LiteDRAM.drawio.png)

*From Ibex to LiteDRAM.*

The *ibex_wb* repository no longer appears to be actively maintained. I looked long and hard at the implementation of the two modules and ultimately decided that I couldn't figure out the author's reasoning. I decided to re-implement both modules: 

- [Core2wb](https://github.com/epsilon537/ibex_wb/blob/master/rtl/core2wb.sv) has two states: *Idle* and *Transaction Ongoing*. In the Idle state, when Ibex signals a transaction request (core.req), the Ibex memory interface signals get registered, a single access pipelined Wishbone transaction is generated and *core2wb* goes to *Transaction Ongoing* state. When a WB ACK or ERR response is received, core2wb goes back to idle. While *Transaction Ongoing* state, the memory interface grant (*gnt*) signal is held low, so further transaction requests are stalled until *core2wb* is idle again Multiple outstanding transactions are currently not supported. I hope to add that capability someday.

![Core2WB State Diagram.](assets/core2wb_fsm.drawio.png)

*Core2WB State Diagram.* 

- [WB_interconnect_shared_bus](https://github.com/epsilon537/ibex_wb/blob/master/soc/common/rtl/wb_interconnect_sharedbus.sv) also has two states: In the _Idle_ state, a priority arbiter monitors the CYC signal of participating Bus Masters. When one or more Bus Masters assert CYC, the arbiter grants access to the lowest order Bus Master and goes to *Transaction Ongoing* state. When that Bus Master de-asserts CYC again, we go back to Idle state. Slave selection and forwarding of WB signals is done with combinatorial logic.

![WB_Interconnect_Shared_Bus State Diagram.](assets/wb_shared_bus_fsm.drawio.png)

*WB_Interconnect_Shared_Bus State Diagram.* 

#### Ibex Core Configuration

I settled on RISCV configuration **RV32IMCB**: The **(I)nteger** and **(C)ompressed** instruction set are fixed in Ibex. **(M)ultiplication and Division** and **(B)it Manipulation** are enabled optional extensions.
Note that there's no Instruction or Data Cache. Code executes directly from DPRAM or DDR memory. Data access also goes straight to DPRAM or DDR memory.
The Ibex core is instantiated with the following *M* and *B* parameters, as shown in the *Ibex_wb* *ibex_soc* example:

**sub/ibex_wb/soc/fpga/arty-a7-35/rtl/ibex_soc.sv**:
```
wb_ibex_core #(
  .RV32M(ibex_pkg::RV32MFast),
  .RV32B(ibex_pkg::RV32BBalanced),
  ...
  ) wb_ibex_core (
  ...  
  .boot_addr    (32'h0),
  ...
```

*Wb_ibex_core*'s *boot_addr* port is a misnomer. *Boot_addr* specifies the base address of the vector table. The Reset Vector, i.e. the first instruction executed when the processor comes out of reset, is at offset 0x80 relative to this base address. Hence, the *real* Boot Address for BoxLambda is address 0x80.

##### Ibex Clock Frequency

The Ibex core is part of the 50MHz System Clock Domain.

### Debug Unit: *RISCV-DBG*

[https://github.com/epsilon537/riscv-dbg](https://github.com/epsilon537/riscv-dbg) forked from [https://github.com/pulp-platform/riscv-dbg](https://github.com/pulp-platform/riscv-dbg).

#### RISCV OpenOCD

OpenOCD is an open-source software package used to interface with a hardware debugger's JTAG port via one of many transport protocols. In our case, the hardware debug logic is implemented by a component called **riscv-dbg**. The overall setup looks like this:

![OpenOCD General Setup](assets/OpenOCD_Setup_General.drawio.png)

*OpenOCD General Setup*

BoxLambda uses the RISCV fork of OpenOCD: [https://github.com/riscv/riscv-openocd](https://github.com/riscv/riscv-openocd)

#### The RISCV-DBG component

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

#### RISCV-DBG RTL Structure

RISCV-DBG has two top-levels:

- [sub/riscv-dbg/src/dm_top.sv](https://github.com/epsilon537/riscv-dbg/blob/b241f967f0dd105f7c5e020a395bbe0ec54e40e4/src/dm_top.sv)
- [sub/riscv-dbg/src/dmi_jtag.sv](https://github.com/epsilon537/riscv-dbg/blob/b241f967f0dd105f7c5e020a395bbe0ec54e40e4/src/dmi_jtag.sv)

Recall that BoxLambda uses a Wishbone interconnect. The Ibex_WB submodule implements a Wishbone wrapper for the Ibex RISCV core. It does the same for RISCV-DBG's *dm_top*:  
[sub/ibex_wb/rtl/wb_dm_top.sv](https://github.com/epsilon537/ibex_wb/blob/87a97e38f3cf15bee80eb69bfa82166c00842b1e/rtl/wb_dm_top.sv)

Refer to the *ibex_soc* example to see how RISCV-DBG is instantiated:  
[sub/ibex_wb/soc/fpga/arty-a7-35/rtl/ibex_soc.sv](https://github.com/epsilon537/ibex_wb/blob/boxlambda/soc/fpga/arty-a7-35/rtl/ibex_soc.sv)

#### OpenOCD and RISCV-DBG on Verilator

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

See the [Test Builds section](installation-and-test-builds.md#test-builds) for the steps needed to set up an OpenOCD JTAG debug session on Verilator.

#### OpenOCD and RISCV-DBG on Arty-A7 FPGA

The obvious approach would be to bring out the JTAG signals to PMOD pins and hook up a JTAG adapter. However, there's an alternative method that doesn't require a JTAG adapter. The riscv-dbg JTAG TAP can be hooked into the FPGA scan chain which is normally used to program the bitstream into the FPGA. On the Arty-A7, bitstream programming is done using the FTDI USB serial port, so no special adapters are needed.

The riscv-dbg codebase lets you easily switch between a variant with external JTAG pins and a variant that hooks into the FPGA scan chain, by changing a single file:

- **dmi_jtag_tap.sv**: hooks up the JTAG TAP to external pins
- **dmi_bscane_tap.sv**: hooks the JTAG TAP into the FPGA scan chain. The Xilinx primitive used to hook into the scan chain do this is called BSCANE. Hence the name.

Both files implement the same module name (*dmi_jtag_tap*) and the same module ports, so you can swap one for the other without further impact on the system. Lightweight polymorphism. It sounds cool, but it feels a bit forced. One annoying consequence is that although the JTAG ports are listed at the top-level, they are not used when building the BSCANE variant. The BSCANE primitive internally provides the actual JTAG ports. Anyway, that's the RISCV-DBG design. I'll go with it. By default, BoxLambda builds the BSCANE variant on the Arty-A7.

On the OpenOCD side, the transport protocol for this Debug-Access-via-FPGA-scan-chain-over-FTDI is anti-climactically called **ftdi**.

![BoxLambda OpenOCD Arty A7 FTDI Setup](assets/OpenOCD_Setup_Arty_A7.drawio.png)

*BoxLambda OpenOCD Arty A7 FTDI Setup*

The OpenOCD configuration file for JTAG Debugging on Arty A7 is checked into the *scripts/* directory: [scripts/digilent_arty_a7.cfg](https://github.com/epsilon537/boxlambda/blob/master/scripts/digilent_arty_a7.openocd.cfg) 

To summarize:

1. The above OpenOCD config file is used to connect to the JTAG TAP of a riscv-dbg core...
2. ...to debug the software running on a connected Ibex RISCV32 core.
3. The riscv-dbg core's JTAG TAP is hooked into the Arty-A7's scan chain, normally used for loading a bitstream into the FPGA.
4. The Arty-A7 FPGA scan chain is accessible through the board's FTDI-based USB serial port.
5. The OpenOCD transport protocol name for this type of connection is **ftdi**.

See the [Test Builds section](installation-and-test-builds.md#test-builds) for the steps needed to set up an OpenOCD JTAG debug session on the Arty A7.

#### RISCV-DBG Clock Frequency

*Dm_top* and *dmi_jtag* are part of the 50MHz System Clock domain.
The JTAG clock *tck* is driven via a *BSCANE2* primitive by the FPGA's JTAG chain. The BSCANE2 primitive is instantiated in the *dmi_bscane_tap* module.
  
### LiteDRAM Memory Controller

SDRAM memory access is pretty complicated. Memory access requests get queued in the memory controller, scheduled, and turned into a sequence of commands that vary in execution time depending on the previous memory locations that were recently accessed. 

There exists a class of memory controllers, called **Static Memory Controllers**, that absorb these complexities and by design create a fixed schedule for a fixed use case, resulting in very predictable behavior. Static Memory Controllers are far off the beaten path, however. **Dynamic Memory Controllers** are more common. Dynamic Memory Controllers can handle a variety of use cases with good performance *on average*. Unfortunately, they sacrifice predictability to achieve this flexibility.

I decided to use the **LiteDRAM** memory controller: [https://github.com/enjoy-digital/litedram](https://github.com/enjoy-digital/litedram)

The LiteDRAM memory controller falls squarely into the Dynamic Memory Controller class. How do we fit this into a platform that requires deterministic behavior? I think the best approach is to use a DMA engine to transfer data between SDRAM and on-chip memory. Fixed memory access latency to on-chip memory (from any bus master that requires it) can be guaranteed using an arbiter.

#### Why choose LiteDRAM over Xilinx MIG?

- LiteDRAM is open-source, scoring good karma points. All the benefits of open-source apply: Full access to all code, access to the maintainers, many eyeballs, the option to make changes as you please, submit bug fixes, etc.
- The LiteDRAM simulation model, the entire test SoC, in fact, runs nicely in Verilator. That's a must-have for me. 
- The LiteDRAM core, configured for BoxLambda, is 50% smaller than the equivalent MIG core: 3016 LUTs and 2530 registers vs. 5673 LUTs and 5060 registers.

#### Generating a LiteDRAM core

LiteDRAM is a highly configurable core. For an overview of the core's features, please take a look at the LiteDRAM repository's README file:

[https://github.com/enjoy-digital/litedram/blob/master/README.md](https://github.com/enjoy-digital/litedram/blob/master/README.md)

You specify the configuration details in a *.yml* file. A Python script parses that *.yml* file and generates the core's Verilog as well as a CSR register access layer for software.

Details are a bit sparse, but luckily example configurations are provided:

[https://github.com/enjoy-digital/litedram/tree/master/examples](https://github.com/enjoy-digital/litedram/tree/master/examples)

Starting from the *arty.yml* example, I created the following LiteDRAM configuration file for BoxLambda:

```
#This is a LiteDRAM configuration file for the Arty A7.
{
    # General ------------------------------------------------------------------
    "speedgrade": -1,          # FPGA speedgrade
    "cpu":        "None",      # CPU type (ex vexriscv, serv, None) - We only want to generate the LiteDRAM memory controller, no CPU.
    "memtype":    "DDR3",      # DRAM type
    "uart":       "rs232",     # Type of UART interface (rs232, fifo) - not relevant in this configuration.

    # PHY ----------------------------------------------------------------------
    "cmd_latency":     0,             # Command additional latency
    "sdram_module":    "MT41K128M16", # SDRAM modules of the board or SO-DIMM
    "sdram_module_nb": 2,             # Number of byte groups
    "sdram_rank_nb":   1,             # Number of ranks
    "sdram_phy":       "A7DDRPHY",    # Type of FPGA PHY

    # Electrical ---------------------------------------------------------------
    "rtt_nom": "60ohm",  # Nominal termination
    "rtt_wr":  "60ohm",  # Write termination
    "ron":     "34ohm",  # Output driver impedance

    # Frequency ----------------------------------------------------------------
    # The generated LiteDRAM module contains clock generation primitives, for its own purposes, but also for the rest
    # of the system. The system clock is output by the LiteDRAM module and is supposed to be used as the main input clock
    # for the rest of the system. I set the system clock to 50MHz because I couldn't get timing closure at 100MHz.
    "input_clk_freq":   100e6, # Input clock frequency
    "sys_clk_freq":     50e6, # System clock frequency (DDR_clk = 4 x sys_clk)
    "iodelay_clk_freq": 200e6, # IODELAYs reference clock frequency

    # Core ---------------------------------------------------------------------
    "cmd_buffer_depth": 16,    # Depth of the command buffer

    # User Ports ---------------------------------------------------------------
    # We generate two wishbone ports, because BoxLambda has two system buses.
    # Note that these are _classic_ wishbone ports, while BoxLamdba uses a _pipelined_ wisbone bus.
    # A pipelined-to-classic wishbone adapter is needed to interface correctly to the bus.
    # At some point it would be nice to have an actual pipelined wishbone frontend, with actual pipelining capability.
    "user_ports": {
        "wishbone_0" : {
            "type":  "wishbone",
            "data_width": 32, #Set data width to 32. If not specificied it defaults to 128 bits.
            "block_until_ready": True,
        },
        "wishbone_1" : {
            "type":  "wishbone",
            "data_width": 32, #Set data width to 32. If not specificied it defaults to 128 bits.
            "block_until_ready": True,
        },
    },
}
```

Some points about the above:

- The *PHY layer*, *Electrical* and *Core* sections I left exactly as-is in the given Arty example.
- In the *General* section, I set *cpu* to *None*. BoxLambda already has a CPU. We don't need LiteX to generate one.
- In the *Frequency* section, I set *sys_clk_freq* to 50MHz. 50MHz has been the system clock frequency in the previous BoxLambda test builds as well. Also, I haven't been able to close timing at 100MHz. 
- In the *User Ports* section, I specified two 32-bit Wishbone ports. In the [BoxLambda Architecture Diagram](https://boxlambda.readthedocs.io/en/latest/architecture/#the-arty-a7-configuration), you'll see that BoxLambda has two system buses. The memory controller is hooked up to both.

I generate two LiteDRAM core variants from this configuration: 

- For simulation: ```litedram_gen artya7dram.yml --sim --gateware-dir sim/rtl --software-dir sim/sw --name litedram```
- For FPGA: ```litedram_gen artya7dram.yml --gateware-dir arty/rtl --software-dir arty/sw --name litedram```

The generated core has the following interface:

```
module litedram (
`ifndef SYNTHESIS    
  input  wire sim_trace, /*Simulation only.*/
`endif
	input  wire clk,
`ifdef SYNTHESIS  
	input  wire rst,       /*FPGA only...*/
	output wire pll_locked,
	output wire [13:0] ddram_a,
	output wire [2:0] ddram_ba,
	output wire ddram_ras_n,
	output wire ddram_cas_n,
	output wire ddram_we_n,
	output wire ddram_cs_n,
	output wire [1:0] ddram_dm,
	inout  wire [15:0] ddram_dq,
	inout  wire [1:0] ddram_dqs_p,
	inout  wire [1:0] ddram_dqs_n,
	output wire ddram_clk_p,
	output wire ddram_clk_n,
	output wire ddram_cke,
	output wire ddram_odt,
	output wire ddram_reset_n,
`endif  
	output wire init_done,  /*FPGA/Simulation common ports...*/
	output wire init_error,
	input  wire [29:0] wb_ctrl_adr,
	input  wire [31:0] wb_ctrl_dat_w,
	output wire [31:0] wb_ctrl_dat_r,
	input  wire [3:0] wb_ctrl_sel,
	input  wire wb_ctrl_cyc,
	input  wire wb_ctrl_stb,
	output wire wb_ctrl_ack,
	input  wire wb_ctrl_we,
	input  wire [2:0] wb_ctrl_cti,
	input  wire [1:0] wb_ctrl_bte,
	output wire wb_ctrl_err,
	output wire user_clk,
	output wire user_rst,
	input  wire [25:0] user_port_wishbone_0_adr,
	input  wire [31:0] user_port_wishbone_0_dat_w,
	output wire [31:0] user_port_wishbone_0_dat_r,
	input  wire [3:0] user_port_wishbone_0_sel,
	input  wire user_port_wishbone_0_cyc,
	input  wire user_port_wishbone_0_stb,
	output wire user_port_wishbone_0_ack,
	input  wire user_port_wishbone_0_we,
	output wire user_port_wishbone_0_err,
	input  wire [25:0] user_port_wishbone_1_adr,
	input  wire [31:0] user_port_wishbone_1_dat_w,
	output wire [31:0] user_port_wishbone_1_dat_r,
	input  wire [3:0] user_port_wishbone_1_sel,
	input  wire user_port_wishbone_1_cyc,
	input  wire user_port_wishbone_1_stb,
	output wire user_port_wishbone_1_ack,
	input  wire user_port_wishbone_1_we,
	output wire user_port_wishbone_1_err
);
```

Some points worth noting about this interface:

- A Wishbone control port is generated along with the two requested user ports. LiteDRAM CSR register access is done through this control port.
- All three Wishbone ports are *classic* Wishbone ports, not *pipelined*. There is no *stall* signal.
- The Wishbone port addresses are word addresses, not byte addresses.
- The LiteDRAM module takes an external input clock (*clk*) and generates a 50MHz system clock (*user_clk*). The module contains a clock generator.
- On FPGA, the LiteDRAM module takes an asynchronous reset (*rst*) and provides a synchronized reset (*user_rst*). The module contains a reset synchronizer.

#### *Litedram_wrapper*

I created a *litedram_wrapper.sv* module around litedram.v:

[https://github.com/epsilon537/boxlambda/blob/master/gw/components/litedram/common/rtl/litedram_wrapper.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/litedram/common/rtl/litedram_wrapper.sv)

This wrapper contains:

- byte-to-word address adaptation on all three Wishbone ports.
- Pipelined-to-Classic Wishbone adaptation. The adapter logic comes straight out of the Wishbone B4 spec section 5.2, *Pipelined master connected to standard slave*. The *stall* signal is used to avoid pipelining:
  
```
  /*Straight out of the Wishbone B4 spec. This is how you interface a classic slave to a pipelined master.
   *The stall signal ensures that the STB signal remains asserted until an ACK is received from the slave.*/
   assign user_port_wishbone_p_0_stall = !user_port_wishbone_p_0_cyc ? 1'b0 : !user_port_wishbone_c_0_ack;
```

##### LiteDRAM Clock Frequency

See [Clocks and Reset](clocks_and_reset.md)

### VERA (Wishbone) Graphics

- **The VERA Wishbone Repo**: 
  [https://github.com/epsilon537/vera_wishbone](https://github.com/epsilon537/vera_wishbone).

- **The VERA Wishbone Submodule in the BoxLambda Directory Tree**: 
  boxlambda/sub/vera_wishbone/.

- **The VERA Gateware Component in the BoxLambda Directory Tree**: 
  [boxlambda/gw/components/vera](https://github.com/epsilon537/boxlambda/tree/master/gw/components/vera)

Frank van den Hoef's [VERA Versatile Embedded Retro Adapter](https://github.com/fvdhoef/vera-module), used by the [Commander X16 project](https://www.commanderx16.com/), is a standalone FPGA with an 8-bit external bus interface. **VERA Wisbone**, discussed here is an adaption of the graphics subsystem of the original VERA core for the BoxLambda SoC.

Note: The original VERA core also supports PSG Audio, PCM Audio, and a SPI Controller for storage. These subcomponents have not been carried over to VERA Wishbone.

#### Features

VERA Wishbone's feature summary:

- 32-bit pipelined Wishbone slave interface.
- VGA output format at a fixed resolution of 640x480@60Hz (same as original VERA).
- Support for 2 layers, both supporting either tile or bitmap mode (same as original VERA).
- Support for 2 banks of 64 sprites. Guaranteed max. of 512 sprite pixels per scanline.
- Configurable Embedded video RAM size of up to 128kB.
- Palette with 256 colors selected from a total range of 4096 colors (same as original VERA).

#### Overview

![The VERA Wishbone Block Diagram.](assets/vera_wishbone.drawio.png)

*The VERA Wishbone Block Diagram.*

In the above diagram, the big arrows show the main data flow. The small arrows are control signals. The labels next to the arrowheads show what is being presented to the block at that point. E.g. the *Composer* presents a *line_idx* and *render_start* signal to the *Layer 0 Renderer*. Data path bus widths are indicated by a number between brackets.

The core operates at **50MHz**, BoxLambda's system clock frequency. However, the **video_vga** and **composer** modules need to run at 25MHz, the 640x480@60Hz VGA pixel clock rate. These blocks use a toggling clock enable signal to do so ([clock enables are preferred over clock dividers](https://electronics.stackexchange.com/questions/222972/advantage-of-clock-enable-over-clock-division)).

The easiest way to understand what's going on is by going through the diagram from bottom to top:

1. The **video_vga** block generates the 640x480@60Hz VGA signal (RGB 4:4:4, Hsync and Vsync).
2. The video_vga block pulls the pixel data from the **Composer** block. The 8-bit pixel data passes through the **palette_ram** to be converted to RGB 4:4:4.
3. The Composer block in turn pulls the necessary data from three **Line Buffers**: one for each layer and one for sprites. The Composer and video_vga blocks operate at VGA pixel clock rate, i.e. 640 pixels worth of data flows when a scanline is being drawn. No data flows during horizontal or vertical retrace.
4. The Line Buffers exist to give the renderers some leeway. The **Layer Renderers**, for instance, need to produce 640 pixels worth of data each scanline but they have 800 pixels worth of time to do so (the horizontal retrace time is 'extra time'). For the **Sprite Renderer**, the numbers are a bit different, but the concept is the same.
5. The renderers contain the bulk of VERA's video generation logic. There are two identical Layer Renderer blocks and one Sprite Renderer. The Layer Renderers implement the different tile and Bitmap Modes, retrieve the necessary data from the **vram_if** block and store the rendered output data in their respective Line Buffers. The Sprite Renderer does the same thing for sprites.
6. The vram_if block contains **128KB or 64KB** of embedded video memory, depending on whether the Arty A7-100T or the Arty A7-35T is being targeted. It has four ports: one for each renderer and one for the CPU (via the external bus). A time slot scheduler gives each port in turn access to VRAM.

#### Video RAM

The video RAM (VRAM) is a Single Port RAM instance of four byte-wide columns (matching a 32-bit Wishbone data bus with four byte-enables). The amount of RAM is configurable, up to 128KB, through a **VRAM_SIZE_BYTES** parameter/macro.

The **vram_if** module has four ports: Two Line Renderers, the Sprite Renderer, and the CPU. The vram_if module arbitrates access to the VRAM using a time slot scheduler. There are four equal time slot *beats*. Each port is assigned one slot during which it can access VRAM. The duration of a timeslot is one clock cycle.

![Time Slot Scheduled VRAM Access.](assets/vram_if_timeslot_scheduling.drawio.png)

*Time Slot Scheduled VRAM Access.*

With this mechanism, bandwidth utilization on one port does not have any impact on any of the other ports. A port that tries to use more than its share of the bus bandwidth is stalled. In practice, the only port where this can happen is the CPU port.

#### The Composer

The Composer receives basic control signals from the video_vga block: *next pixel*, *next line*, *next frame*, *vblank*. It uses these signals for the following purposes:

- Generate control/timing signals towards the other blocks, e.g. *line index*, *render start*, *frame done*, *sprite Line Buffer erase start*.
- Keep track of the horizontal and vertical screen position counters, both regular and scaled.  
- Generate line IRQs.
- Determine the active area of the screen, where the border isn't shown.
- Compose the display, reading out the pixel data from the three Line Buffers.
   
#### The Layer Renderer

The Layer Render's implementation is, conceptually at least, reasonably straightforward:

1. Depending on the scanline index and the current position in the Line Buffer, it retrieves the appropriate map entry from VRAM (assuming Tile Mode).
2. It retrieves the corresponding tile pixel data, also from VRAM.
3. It writes out the pixel data to the next few positions in the Line Buffer.

Steps 1 and 2 are always sequential, but as much as possible they overlap with step 3, i.e. while pixel data is being rendered out to the Line Buffer, new map and/or tile data is being retrieved from VRAM.

Below you see the waveform of the Layer Renderer operating in 8bpp Tile Mode, 8 pixel wide tiles. One full FSM (Finite State Machine) cycle is shown, i.e. the pattern between the two vertical cursors is repeating.
You can see the VRAM reads (*buf_strobe* and *bus_ack*) happening in parallel with the Line Buffer writes (*lb_wridx_r*). You can also see how the Renderer FSM cycles through its states (*fetch map*, *wait fetch map*, *fetch tile*, *wait fetch tile*, *render*, etc.).

![Layer Rendering Waveform.](assets/layer_render_l0_only_tile_mode_8bpp_tile_w_8.jpg)

*Layer Rendering Waveform.*

##### Other Layer Renderer Responsibilities

Other responsibilities of the Layer Renderer include:

- Bitmap Mode: Simpler than Tile Mode, but more expensive in terms of required memory resources. In Bitmap Mode, VRAM holds a frame buffer.
- Handling of vertical and horizontal scrolling.
- Handling of the different colors-depths: 8bpp, 4bpp, 2bpp, 1bpp.
- Handling of the different tile widths and heights.
- Tile V-flip and H-flip.
  
#### The Layer Line Buffer

The Layer Renderer has an 8-bit write-only interface to its Line Buffer. The Line Buffer contains 8 bits per entry. One entry corresponds to one pixel.
The Layer Line Buffer implements a double buffering scheme: While the renderer is writing out one scan line, the Composer is reading out the other line. When they are done with the respective scan lines, they switch places.  
  
#### The Sprite Renderer

The Sprite Renderer's operation is a bit more complicated:

1. The **Sprite Attribute RAM** is scanned front-to-back to find the next sprite ID that is active on the current scan line.
2. When an active Sprite has been found, its pixel data for the current scan line is retrieved from VRAM.
3. The pixel data is written out to the correct positions in the Line Buffer. That means there may be multiple overlapping writes into the Sprite Line Buffer. The Sprite Line Buffer is written in Sprite ID order, at the position determined by the sprite's X-coordinate.

The sequence of scanning the sprite attribute RAM, retrieving sprite pixel data, and rendering it out to the Line Buffer is entirely sequential. There is no pipelining.

In the waveform below, you can see two sprites getting rendered out on a scanline: sprite ID 4 at position 192, and sprite ID 5 at position 256. The two sprites have 8bpp color depth and are 8 pixels wide. You can see that, for each sprite, two VRAM read operations are performed (*bus_strobe* and *bus_ack* signals), and 8 entries are accessed in the Sprite Line Buffer (*linebuf_idx_r*).

![Sprite Rendering Waveform.](assets/spr_render_8bpp_8w.jpg)

*Sprite Rendering Waveform.*

##### Sprite Banks

The Sprite Attribute RAM consists of two banks of 64 sprite IDs. A bit in the *VERA_CTRL* register is used to select the active bank. 

Sprite Banking may help with sprite multiplexing or animation: While one sprite bank is active, software can prepare the inactive bank's entries and switch over at the right moment, triggered by a *line_irq*, for instance.

![Double Buffering with Sprite Banks.](assets/sprite_banking_double_buffering.drawio.png)

*Double Buffering with Sprite Banks Example.*

![Sprite Multiplexing with Sprite Banks.](assets/sprite_banking_muxing.drawio.png)

*Sprite Multiplexing with Sprite Banks Example.*

Note: Sprite Banking does not exist in the original VERA core. The original VERA core uses a single bank of 128 Sprite IDs.

##### A Fixed Sprite-Pixels-per-Scanline Limit

The Sprites-per-Scanline limit is inversely proportional to the sprite width. That makes sense. It takes roughly twice as long to render a 16-pixel-wide sprite than an 8-pixel-wide sprite. Conversely, the number of *sprite pixels* that can be rendered on a given scanline is relatively constant. For VERA Wishbone running at 50MHz, this constant is 512 pixels, i.e. the Sprite Renderer can render a maximum of 512 sprite pixels on any scanline, guaranteed.

The original Sprite Renderer code kept track of rendering time to decide when to abort rendering, to avoid exceeding its time budget. VERA Wishbone replaces that code with logic that keeps track of the number of sprite pixels rendered. When 512 sprite pixels are rendered, further sprite rendering is aborted for the given scanline.

![512 Sprite Pixel Limit Examples.](assets/sprite_pixel_limit_examples.drawio.png)

*Examples of the 512 Sprite Pixel Limit.*

The VERA Wishbone changes relative to the original VERA code make it easier for a programmer to plan sprites. The programmer knows ahead of time exactly how many sprites of a given size he can have on the same scanline. This number is independent of the Layer Renderer settings and the VRAM loading by the CPU (or any other Wishbone bus master accessing VRAM, such as a DMA core).

##### Other Sprite Renderer Responsibilities

Other responsibilities of the Sprite Renderer include:

- Decoding and handling of sprite attributes: color-depth, width, height, z-depth, and collision mask.
- Sprite-to-Sprite Collision Detection.
- Handling pixel transparency.

#### The Sprite Line Buffer

The Sprite Line Buffer contains 16 bits per entry. One entry corresponds to one pixel. Each entry contains the sprite pixel's collision data and z-value (layering depth relative to layers 0 and 1), along with the pixel data. The Sprite Renderer's collision handling logic has to be able to read back the collision data of the sprite pixels that are already rendered out, so the Sprite Renderer has a 16-bit read-and-write interface to its Line Buffer.

Like the Layer Line Buffer, the Sprite Line Buffer implements a double buffering scheme.

The Sprite Line Buffer also contains erase logic. When the Composer has read one scanline, it sends a signal to the Sprite Line Buffer to erase that line. Erasing is necessary because the Sprite Renderer doesn't necessarily write to each position in the line buffer. If the line were not erased, stale pixels (and associated meta-data) from a previous scanline may shine through.

#### Wishbone Interface and Memory Mapped Access

The VERA Wishbone core has a pipelined Wishbone slave interface. The interface has a 32-bit data port, 4-byte lane enables, and a 17-bit word-addressed address port.

In the original VERA core, VRAM, Palette RAM, and Sprite RAM were accessed indirectly through its register interface. In VERA Wishbone, this indirect access method has been replaced with a memory-mapped interface.

The following table shows the memory-mapped address ranges:

| Address Range           | Description       | Access     |
| ----------------------- | ----------------- | ---------- |
| 0x10100000 - 0x10100100 | VERA Registers    | Read/Write |
| 0x10101000 - 0x10101400 | Sprite attributes | Write Only |
| 0x10102000 - 0x10102200 | Palette           | Write Only |
| 0x10140000 - 0x10160000 | Video RAM (128KB) | Read/Write |

Note:

- The above addresses are absolute, as seen by the processor. The VERA core's base address is 0x10100000.
- The Video RAM address range depends on the amount of Video RAM set by the *VRAM_SIZE_BYTES* macro. The range 0x10140000-0x10160000 corresponds to a *VRAM_SIZE_BYTES* setting of 131072 (128K).

#### Register Interface / Programmer's Reference.

For a description of the VERA Wishbone registers, refer to the [VERA Programmer's Reference](https://github.com/epsilon537/vera_wishbone/blob/boxlambda/doc/VERA%20Programmer's%20Reference.md).

#### Top-Level Interface and Output Pins 

The *vera_wishbone* top-level interface is straightforward:

```
module vera_top #(
	parameter VRAM_SIZE_BYTES=(128*1024) //Max. 128KB.
	)
  (
  input  wire       clk,
  input  wire       reset,

  //32-bit pipelined Wishbone interface.
  input wire [16:0]  wb_adr,
  input wire [31:0]  wb_dat_w,
  output wire [31:0] wb_dat_r,
  input wire [3:0]   wb_sel,
  output wire        wb_stall,
  input wire         wb_cyc,
  input wire         wb_stb,
  output wire        wb_ack,
  input wire         wb_we,
  output wire        wb_err,

  // IRQ
  output wire        irq_n,

  // VGA interface
  output reg  [3:0]  vga_r,       
  output reg  [3:0]  vga_g,       
  output reg  [3:0]  vga_b,       
  output reg         vga_hsync,   
  output reg         vga_vsync   
  );
```

It assumed that [Diligent's VGA PMOD](https://digilent.com/reference/pmod/pmodvga/start) is being used. The VGA signals are sent to PMOD ports JB and JC with the following pin layout:

| Pin       | Description | Pin       | Description |
| --------- | ----------- | --------- | ----------- |
| JB Pin 1  | R0          | JC Pin 1  | G0          |
| JB Pin 2  | R1          | JC Pin 2  | G1          |
| JB Pin 3  | R2          | JC Pin 3  | G2          |
| JB Pin 4  | R3          | JC Pin 4  | G3          |
| JB Pin 5  | GND         | JC Pin 5  | GND         |
| JB Pin 6  | VCC         | JC Pin 6  | VC3V3       |
| JB Pin 7  | B0          | JC Pin 7  | HS          |
| JB Pin 8  | B1          | JC Pin 8  | VS          |
| JB Pin 9  | B2          | JC Pin 9  | NC          |
| JB Pin 10 | B3          | JC Pin 10 | NC          |
| JB Pin 11 | GND         | JC Pin 11 | GND         |
| JB Pin 12 | VCC3V3      | JC Pin 12 | VCC         |

### Sound

A sound core is a perfect candidate for Partial FPGA Reconfiguration. There are a lot of options (Wave-Table synthesis, FM synthesis, PSG...) and a lot of open-source cores available. It would be pretty cool if the software application can just download its synthesizer of choice as part of the program.

Pretty much any core developed by [Jotego](https://github.com/jotego) sounds like a great idea.

Technically, I don't have to select a sound core. We already have sound through VERA's PCM audio playback. I'm going to select a sound core anyway because I like retro sounds and I'd like to mess around a bit with one of the old-school PSG chips. 

I think I'll go for a dual [**YM2149**](https://en.wikipedia.org/wiki/General_Instrument_AY-3-8910), one for music, one for sound FX, in a game context. The YM2149 was the Atari ST's sound chip, so we'll have a large music and sound FX archive at our disposal. Jotego developed an FPGA clone of the YM2149, the JT49:

[https://github.com/jotego/jt49](https://github.com/jotego/jt49)

### DMA

I was on the fence for a while, deciding whether or not I should include a DMA engine in our machine. In a previous post, I said I would use DMA to move data between external and internal memory. However, a DMA Controller is by definition a bus master, and having multiple bus masters (DMAC and CPU) adds significant complexity to the architecture: access to shared buses and slaves, impact on timing, etc. In a system with only one bus master, the CPU, you don't have to worry about any of that.

Then I snapped out of it and remembered that BoxLambda is intended to be a platform for RTL experimentation. It would be silly to restrict these RTL experiments to bus slave components only. In other words, the BoxLambda architecture is going to have to accommodate bus masters, so we might as well include a DMA Controller.

Some use cases for DMA in the scope of our computer include:

- Moving data between external (DDR) and internal (Block RAM) memory.
- Streaming from memory to the audio DAC.
- Blitting, i.e. copying data into video memory, taking into account the video memory's organization. For instance, copying a rectangular block of data into a frame buffer requires striding between rows of pixel data. Another example: Bit planes with 1, 2, or 4 bits-per-pixel color depths require barrel shifting when copying data to a specific pixel offset.

I spent many hours online searching for DMA Controllers. I was a bit surprised that there were so few options, so I kept digging. I found [ZipCPU's](https://github.com/ZipCPU/zipcpu/blob/master/rtl/peripherals/wbdmac.v), [FreeCore's](https://github.com/stffrdhrn/wb_dma), and [Ant Micro's](https://github.com/antmicro/fastvdma) DMA controllers. The Anti Micro DMAC seemed to be the most interesting option, with two Wishbone ports, pipelined mode, striding support, and support for any byte boundary alignment.

I was ready to go with the Ant Micro selection when I happened across an old post on Reddit where somebody proposed a 'smart' DMA concept: a DMAC with a tiny CPU embedded in it. That sounded like a great concept, so I pinged the author to check what became of his idea. In response, the author generously decided to release his code on GitHub! The core is called **Praxos**. Here is the repository:

[https://github.com/esherriff/Praxos](https://github.com/esherriff/Praxos)

Praxos has a tiny CPU with a small amount of program and data memory embedded in the core, allowing you to write microcode specifying the DMA behavior you want: word/non-word alignment, incrementing/decrementing/non-incrementing source and/or destination address, strides between transfers, combining sources, barrel shifting... Maximum flexibility!

It's not perfect though. Praxos only has one bus master port, an Avalon port at that. It should be doable to slap a standard Wishbone port onto it, but in its current form, I think it won't be able to take advantage of Wishbone's pipelined burst mode. That's unfortunate for a DMAC. 

Still, having the option to hack together my own application-specific DMA microcode sounds like a lot of fun. I just have to go with the Praxos option.

Many thanks to esherriff for making his code available!

### Storage

I'm going to use ZipCPU's SD Card Controller in combination with the FatFs software library to mount a FAT filesystem on the SD card:

- SD Card Controller: [https://github.com/ZipCPU/sdspi](https://github.com/ZipCPU/sdspi)
- FatFs library: [http://elm-chan.org/fsw/ff/00index_e.html](http://elm-chan.org/fsw/ff/00index_e.html)

The SD Card Controller has a Wishbone slave port.

### Keyboard and Mouse

FreeCores has PS/2 keyboard and mouse modules: [https://github.com/freecores/ps2](https://github.com/freecores/ps2)

These cores don't have a Wishbone slave port, so we're going to have to add that ourselves.

### I2C

The I2C interface can be used to hook up a [Real-Time Clock PMOD](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/) as well as a [Wii Nunchuck Adapter](https://www.reichelt.com/be/en/arduino-8211-wiichuck-nunchuck-adapter-ard-wii-nunchuck-p282673.html?CCOUNTRY=661&LANGUAGE=nl&GROUPID=9020&START=0&OFFSET=16&SID=93757c8e4582e90848068d74dbb71d4a2c938ebd13432dc6b9c96&LANGUAGE=EN&&r=1).

ZipCPU has an I2C core with a Wishbone port: [https://github.com/ZipCPU/wbi2c](https://github.com/ZipCPU/wbi2c).

### Serial Port

ZipCPU comes to the rescue once again with a UART implementation with a Wishbone interface: [https://github.com/ZipCPU/wbuart32](https://github.com/ZipCPU/wbuart32)

Location of the *wbuart32* submodule in the BoxLambda repository: **boxlambda/sub/wbuart32/**

### Miscellaneous Modules

- **DFX Controller**: The actual loading of a Reconfigurable Module into a Reconfigurable Partition is handled by the DFX Controller. DFX stands for **Dynamic Function Exchange** which is Xilinx-speak for Partial FPGA Reconfiguration.
- **ICAP**: Internal Configuration Access Port. This module gives access to the FPGA configuration functionality built into Xilinx FPGAs. We'll use the ICAP to implement in-system updates of the Full Configuration Bitstream, loaded into the FPGA upon boot-up.
- **Quad SPI Flash**: This is a module provided by Xilinx, giving access to the Flash Memory device attached through a Quad-SPI bus. The non-volatile Flash Memory will hold the Full Configuration Bitstream(s), System Firmware, and non-volatile system configuration parameters such as keyboard type
- **wb_gpio**: A simple GPIO core with a Wishbone interface, for sampling buttons and switches, and driving LEDs. *wb_gpio.v* is included in the *Ibex_WB* submodule.

