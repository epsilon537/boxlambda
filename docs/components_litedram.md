---
hide:
  - toc
---

## LiteDRAM Memory Controller

- **LiteX Repo**, BoxLambda fork, `boxlambda` branch:
  [https://github.com/epsilon537/litex](https://github.com/epsilon537/litex).

- **LiteX Submodule in the BoxLambda Directory Tree**:
  boxlambda/sub/litex/.

- **LiteDRAM Component in the BoxLambda Directory Tree**:
  [boxlambda/gw/components/litedram](https://github.com/epsilon537/boxlambda/tree/master/gw/components/litedram)

SDRAM memory access is pretty complicated. Memory access requests get queued in the memory controller, scheduled, and turned into a sequence of commands that vary in execution time depending on the previous memory locations that were recently accessed.

There exists a class of memory controllers, called **Static Memory Controllers**, that absorb these complexities and by design create a fixed schedule for a fixed use case, resulting in very predictable behavior. Static Memory Controllers are far off the beaten path, however. **Dynamic Memory Controllers** are more common. Dynamic Memory Controllers can handle a variety of use cases with good performance *on average*. Unfortunately, they sacrifice predictability to achieve this flexibility.

I decided to use the **LiteDRAM** memory controller: [https://github.com/enjoy-digital/litedram](https://github.com/enjoy-digital/litedram)

The LiteDRAM memory controller falls squarely into the Dynamic Memory Controller class. How do we fit this into a platform that requires deterministic behavior? I think the best approach is to use a DMA engine to transfer data between SDRAM and on-chip memory. Fixed memory access latency to on-chip memory (from any bus master that requires it) can be guaranteed by using Dual Port Memory and a crossbar.

### Why choose LiteDRAM over Xilinx MIG?

- LiteDRAM is open-source, scoring good karma points. All the benefits of open-source apply: Full access to all code, access to the maintainers, many eyeballs, the option to make changes as you please, submit bug fixes, etc.
- The LiteDRAM simulation model, the entire test SoC, in fact, runs nicely in Verilator. That's a must-have for me.
- The LiteDRAM core, configured for BoxLambda, is almost 50% smaller than the equivalent MIG core.

### Generating a LiteDRAM core

LiteDRAM is a highly configurable core. For an overview of the core's features, please take a look at the LiteDRAM repository's README file:

[https://github.com/enjoy-digital/litedram/blob/master/README.md](https://github.com/enjoy-digital/litedram/blob/master/README.md)

The configuration details are specifief in a `.yml` file. A Python script parses that `.yml` file and generates the core's Verilog as well as a CSR register access layer for software.

Starting from [LiteDram's arty.yml](https://github.com/enjoy-digital/litedram/tree/master/examples/arty.yml) example, I created the following LiteDRAM configuration file for BoxLambda:

```
{
    # General ------------------------------------------------------------------
    "speedgrade": -1,          # FPGA speedgrade
    "cpu":        "None",  # CPU type (ex vexriscv, serv, None) - We only want to generate the LiteDRAM memory controller, no CPU.
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
    "input_clk_freq":  100e6, # Input clock frequency
    "sys_clk_freq":     50e6, # System clock frequency (DDR_clk = 4 x sys_clk)
    "iodelay_clk_freq": 200e6, # IODELAYs reference clock frequency

    # Core ---------------------------------------------------------------------
    "cmd_buffer_depth": 16,    # Depth of the command buffer

    # User Port ---------------------------------------------------------------
    # Note that this is a _classic_ wishbone port, while BoxLamdba uses a _pipelined_ wisbone bus.
    # A pipelined-to-classic wishbone adapter is needed to interface correctly to the bus.
    # At some point it would be nice to have an actual pipelined wishbone frontend, with actual pipelining capability.
    "user_ports": {
        "wishbone_0" : {
            "type":  "wishbone",
            "data_width": 32, #Set data width to 32. If not specified, it defaults to 128 bits.
            "block_until_ready": True,
        }
    },
}
```

Some points about the above:

- The *PHY layer*, *Electrical*, and *Core* sections I left exactly as-is in the given Arty example.
- In the *General* section, I set `cpu` to `None`. BoxLambda already has a CPU. We don't need LiteX to generate one.
- In the `Frequency` section, I set `sys_clk_freq` to 50MHz.
- In the `User Ports` section, I specified one 32-bit Wishbone port.

Two LiteDRAM core variants are generated from this configuration:

- For simulation:

```litedram_gen artya7dram.yml --sim --gateware-dir sim/rtl --software-dir sim/sw --name litedram```

- For FPGA:

```litedram_gen artya7dram.yml --gateware-dir arty/rtl --software-dir arty/sw --name litedram```

The LiteDRAM cores are generated during the code generation step of a gateware build. The generated files are written to the `<build_tree/codegen/litedram/` directory.

This is the build script performing the code generation: [scripts/gen_litedram_core.sh](https://github.com/epsilon537/boxlambda/blob/master/scripts/gen_litedram_core.sh)

### LiteDRAM Interface

The generated core has the following interface:

```
module litedram (
`ifndef SYNTHESIS
    input  wire sim_trace, /*Simulation only.*/
`endif
`ifdef SYNTHESIS
    output wire          pll_locked, /*FPGA only...*/
    input  wire          rst,
    input  wire          clk,
    output wire   [13:0] ddram_a,
    output wire    [2:0] ddram_ba,
    output wire          ddram_cas_n,
    output wire          ddram_cke,
    output wire          ddram_clk_n,
    output wire          ddram_clk_p,
    output wire          ddram_cs_n,
    output wire    [1:0] ddram_dm,
    inout  wire   [15:0] ddram_dq,
    inout  wire    [1:0] ddram_dqs_n,
    inout  wire    [1:0] ddram_dqs_p,
    output wire          ddram_odt,
    output wire          ddram_ras_n,
    output wire          ddram_reset_n,
    output wire          ddram_we_n,
`endif
    output wire          init_done, /*FPGA/Simulation common ports...*/
    output wire          init_error,
    output wire          user_clk,
    output wire          user_clkx2,
    output wire          user_port_wishbone_0_ack,
    input  wire   [25:0] user_port_wishbone_0_adr,
    input  wire          user_port_wishbone_0_cyc,
    output wire   [31:0] user_port_wishbone_0_dat_r,
    input  wire   [31:0] user_port_wishbone_0_dat_w,
    output wire          user_port_wishbone_0_err,
    input  wire    [3:0] user_port_wishbone_0_sel,
    input  wire          user_port_wishbone_0_stb,
    input  wire          user_port_wishbone_0_we,
    output wire          user_rst,
    output wire          wb_ctrl_ack,
    input  wire   [29:0] wb_ctrl_adr,
    input  wire    [1:0] wb_ctrl_bte,
    input  wire    [2:0] wb_ctrl_cti,
    input  wire          wb_ctrl_cyc,
    output wire   [31:0] wb_ctrl_dat_r,
    input  wire   [31:0] wb_ctrl_dat_w,
    output wire          wb_ctrl_err,
    input  wire    [3:0] wb_ctrl_sel,
    input  wire          wb_ctrl_stb,
    input  wire          wb_ctrl_we
);
```

Some points worth noting about this interface:

- A Wishbone control port is generated along with the user port. LiteDRAM CSR register access is done through the control port.
- Both Wishbone ports are *classic* Wishbone ports, not *pipelined*. There is no `stall` signal.
- The Wishbone port addresses are word addresses, not byte addresses.
- The LiteDRAM module takes an external input clock (`clk`) and generates both a 50MHz system clock (`user_clk`) and a 100MHz double-rate system clock (`user_clkx2`). The LiteDRAM module contains a PLL clock primitive.
- The double-rate system clock is a modification for BoxLambda, but is currently not used.

### *Litedram_wrapper*

[https://github.com/epsilon537/boxlambda/blob/master/gw/components/litedram/common/rtl/litedram_wrapper.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/litedram/common/rtl/litedram_wrapper.sv)

`Litedram_wrapper` contains Pipelined-to-Classic Wishbone adaptation. The adapter logic comes straight out of the Wishbone B4 spec section 5.2, *Pipelined master connected to standard slave*. The `stall` signal is used to avoid pipelining:

```
  /*Straight out of the Wishbone B4 spec. This is how you interface a classic slave to a pipelined master.
   *The stall signal ensures that the STB signal remains asserted until an ACK is received from the slave.*/
   assign user_port_wishbone_p_0_stall = !user_port_wishbone_p_0_cyc ? 1'b0 : !user_port_wishbone_c_0_ack;
```

#### LiteDRAM Clock Frequency

See [Clocks and Reset](clocks_and_reset.md)
