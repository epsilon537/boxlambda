---
hide:
  - toc
---

# The Linker Script

Through a *linker script*, we tell the linker where in memory to place the program code, data, and stack.

The Linker Script defines the following:

- Relevant Memories on the target device: In the case of BoxLambda, these are `imem` and `emem` (=DDR memory).
```
MEMORY
{
    imem : ORIGIN = __imem, LENGTH = __imem_size
    emem : ORIGIN = __emem, LENGTH = __emem_size
}
```
- The mapping of input to output sections. Input sections are defined in the source code and default to .text, .bss, and .data when not explicitly specified. The output sections for BoxLambda are: `.text`, `.data`, `.tdata`, `.tbss`, `.bss`, `.heap`, and `.stack`.
```
    .text : {
        ...
        *(.text.unlikely .text.unlikely.*)
        *(.text.startup .text.startup.*)
        *(.text .text.*)
        *(.gnu.linkonce.t.*)
    ...
```
- The mapping of output sections to memories:
```
    ...
    .text : {...
    } >imem
    ...
    .data : ALIGN_WITH_INPUT {...
    } >imem
    ...
    .bss (NOLOAD) : {...
    } >imem
    ...
    .heap (NOLOAD) : {...
    } >emem
```
  - Code, Data, BSS, and stack sections go to `imem`.
  - The heap goes to `emem`.
- Symbols used by the CRT0 code for section relocation, BSS initialization, etc. For BoxLambda, the key symbols are:
    - `__code_source / __code_start / __code_size`: source address, destination address, and size of the code section. In the Boot-from-IMEM sequence, `__code_source` and `__code_start` point to the same IMEM address.
    - `__data_source / __data_start / __data_size`: source address, destination address, and size of the data section. In the Boot-from-IMEM sequence; `__data_source` and `__data_start` point to the same IMEM address.
    - `__bss_start / __bss_size`: Address and size of BSS section in IMEM to zero out.
```
    .text : {
        PROVIDE(__code_start = ADDR(.text));
        ...
        PROVIDE(__code_end = .);
    } >imem
    PROVIDE(__code_source = LOADADDR(.text));
    PROVIDE(__code_size = __code_end - __code_start );
    ...
```

Currently, with the exception of the Stage 2 Bootloader, all BoxLambda executables execute from IMEM and use the following linker script:

[sw/components/bootstrap/link_imem_boot.ld](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/link_imem_boot.ld)

The Stage 2 Bootloader executes from SDRAM and is built using the `link_ddr_boot.ld` linker script:

[sw/components/bootstrap/link_ddr_boot.ld](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/link_ddr_boot.ld)

See the [Bootloader section](sw_comp_bootloader.md) for more details.

