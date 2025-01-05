## The Wishbone Interconnect

- **Crossbar Repo**, BoxLambda fork, *boxlambda* branch: 
  [https://github.com/epsilon537/wb2axip](https://github.com/epsilon537/wb2axip).

- **Crossbar Submodule in the BoxLambda Directory Tree**: 
  boxlambda/sub/wb2axip/.

- **Crossbar Component in the BoxLambda Directory Tree**: 
  [boxlambda/gw/components/wbxbar](https://github.com/epsilon537/boxlambda/tree/master/gw/components/wbxbar)

- **Crossbar Top-Level**:
[sub/wb2axip/rtl/wbxbar.v](https://github.com/epsilon537/wb2axip/blob/boxlambda/rtl/wbxbar.v)

The Bus, or Interconnect, is the fabric stitching together the SoC internal components. For this project, the two most relevant SoC internal bus specifications are [ARM's AXI bus](https://developer.arm.com/documentation/ihi0022/latest) and the Open-Source [Wishbone bus](https://wishbone-interconnect.readthedocs.io/en/latest/).

**AXI** is very powerful, very popular, and very complex. It scales up well to very big SoCs. However, I don't think it scales down very well to simple SoCs, such as BoxLambda, where low latency and low complexity are more important than high bandwidth and scalability. Hence, for this project, I'm going with **Wishbone**. 

We'll be using Wishbone in *Pipelined Mode*, as specified in the [Wishbone B4 specification](https://github.com/fossi-foundation/wishbone/blob/master/documents/spec/wbspec_b4.pdf).

### Crossbar, Shared Bus, and Bus Arbiter

BoxLambda uses a *Crossbar*, a *Shared Bus*, and in case of the DFX Configuration also a *Bus Arbiter*. Refer to the [Architecture](architecture.md#architecture) section to see how these components fit in the overall architecture.

### Word Addressing

BoxLambda uses Word Addressing, as opposed to Byte Addressing.

![Byte vs. Word Addressing.](assets/byte_vs_word_addressing.png)

*Byte Addressing (left) vs. Word Addressing (right).*

Note that you can still address a byte using a Word Addressing bus. That's what the Byte Enables are for:

![Word Addressing a byte with byte enables.](assets/addressing_a_byte_w_byte_enables.png)

*Addressing a byte using Word Addressing and Byte Enables.*

### Bus Width and Clock Frequency

BoxLambda's Interconnect uses a 32-bit data bus and a 28-bit word address bus. 

The Interconnect is part of the 50MHz System Clock Domain.

### WBXbar and WBArbiter

I'm using ZipCPU's [WBXbar](https://github.com/epsilon537/wb2axip/blob/boxlambda/rtl/wbxbar.v) module for both the crossbar and the shared bus. A crossbar instance with one bus master port is equivalent to a shared bus instance with one bus master port.

The WBXbar crossbar module is well-documented in this ZipCPU Blog post:

[https://zipcpu.com/blog/2019/07/17/crossbar.html](https://zipcpu.com/blog/2019/07/17/crossbar.html)

The Wishbone Bus Arbiter, [WBArbiter](https://github.com/epsilon537/wb2axip/blob/boxlambda/rtl/wbarbiter.v), comes from the same ZipCPU *wb2axip* repo.

### Invalid Addresses

I added one small 'feature' to WBXbar (not everyone will agree that this is a feature): Wishbone transactions to an invalid address are acknowledged. The Wishbone error, which normally would be strobed in such a situation, is surpressed. Reading from an invalid address returns 0xDEADBEEF.

This feature is enabled with the *OPT_ACK_INVALID_ADDR* and *OPT_TIMEOUT* parameters of WBXbar. Specifically:

- OPT_ACK_INVALID_ADDR causes Wishbone accesses to addresses that fall outside the configured address map to get acknowledged. The return data is set to ERROR_DATA_PATTERN, also a WBXbar parameter.
- OPT_TIMEOUT is set to 511. This causes WBXbar to abort a Wishbone transaction after 511 clock cycles. This is used to terminate transactions to not responding slaves.

The idea is that when I'm hacking away on the REPL, I don't want to trigger an unrecoverable CPU exception when I accidentally peek or poke an invalid address.

In test builds I do want invalid addresses to cause Wishbone errors, so I defined a top-level flag to control the invalid address behavior. The flag is called **ACK_INVALID_ADDR**. All test builds except the *invalid_address* test build have this flag unset 0. The project builds *boxlambda_base* and *boxlambda_dfx* and the *invalid_address* test build have this flag set.


