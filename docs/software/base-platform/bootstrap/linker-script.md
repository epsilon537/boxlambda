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
- The mapping of input to output sections. Input sections are defined in the source code and default to `.text`, `.bss`, and `.data` when not explicitly specified. Typical output sections for BoxLambda are: `.etext`, `.edata`, `ebss`, `.itext`, `.idata`, `.tdata`, `.tbss`, `.ibss`, `.heap`, and `.stack`.
```
    .itext : {
        ...
        /* code */
        *(.text.unlikely .text.unlikely.*)
        *(.text.startup .text.startup.*)
        *(.text .text.*)
        *(.gnu.linkonce.t.*)
        ...
```
- The mapping of output sections to memories:
```
    ...
    .itext : {...
    } >imem
    ...
    .idata : ALIGN_WITH_INPUT {...
    } >imem
    ...
    .ibss (NOLOAD) : {...
    } >imem
    ...
    .heap (NOLOAD) : {...
    } >emem
```
  - Code, Data, and BSS input sections go to `imem` or `emem` depending on their assignment to *.itext/data/bss* or *.etext/data/bss* output sections.
  - The stack typically goes to `imem`.
  - The heap goes to `emem`.
- Symbols used by the CRT0 code for section relocation, BSS initialization, etc. For BoxLambda, the key symbols are:
    - `__icode_source / __icode_start / __icode_end`: source address, destination start address, and destination end address of the IMEM code section. In the `link_imem_boot.ld` case, `__icode_source` and `__icode_start` point to the same IMEM address. In the `link_ddr_boot.ld` case, `__icode_source` points to EMEM and `__icode_start` points to IMEM.
    - `__ecode_source / __ecode_start / __ecode_end`: source address, destination start address, and destination end address of the EMEM code section.
    - `__idata_source / __idata_start / __idata_end`: source address, destination start address, and destination end address of the IMEM data section. In the `link_imem_boot.ld` case ; `__data_source` and `__data_start` point to the same IMEM address. In the `link_ddr_boot.ld` case ; `__idata_source` and `__idata_start` point to the same IMEM address.
    - `__edata_source / __edata_start / __edata_end`: source address, destination start address, and destination end address of the EMEM data section.
    - `__ibss_start / __ibss_end`: Start and end address of the BSS section in IMEM to zero out.
    - `__ebss_start / __ebss_size`: Start and end address of the BSS section in EMEM to zero out.
```
    .itext : {
       ...
    } >imem

    PROVIDE(__icode_end = .);
    PROVIDE( __icode_start = ADDR(.itext) );
    PROVIDE( __icode_source = ADDR(.itext) );
```

### Why no Linker-Defined Size Variables?

Linker-defined variables such as `__icode_start` are not true variables. They are address markers only, not storage objects. For example, it's meaningless to get the value of `__icode_start`. What you want is its *address*, i.e. `&__icode_start`. This address corresponds to the start of the *icode* section.

This is also true for linker-defined size variables. For example, if `__icode_size` is linker-defined as:

```
PROVIDE( __icode_size = __icode_end - icode_start)
```

The correct way to get the *icode* section's size value is by taking the **address** of the `__icode_size` variable, i.e `&__icode_size`. I find that quite confusing, so as much as possible, I avoid linker-defined size variables. Instead, I create *__xxx_start* and *__xxx_end* linker variables and I compute the size in code as `&__xxx_end - &__xxx_start`. For example:

```
local_memcpy(&__icode_start, &__icode_source, &__icode_end - &__icode_start);
```

What would happen if you did create an `__icode_size` linker variable and reference in C by value, e.g. `(unsigned)__icode_size`? It might work, until it didn't. One issue is that the compiler might consider the `__icode_size` variable access as eligible for *Small Data Access* optimization. In that case, the compiler will attempt to generate code using global pointer (*gp*) relative addressing. The problem with that is that `__icode_size` is not an object in the gp-relative addressable range. In fact, `__icode_size` not a true object at all. The compilation will abort with an error message.

## Linker Script Variants

- [bootstrap/link_imem_boot.ld](../../../../sw/components/bootstrap/link_imem_boot.ld) creates a test image that boots directly from IMEM.
- [bootstrap/link_ddr_boot.ld](../../../../sw/components/bootstrap/link_ddr_boot.ld) creates an application or test image to be loaded into EMEM by the bootloader. From there, the image will unpack itself into IMEM.
- [bootloader/link.ld](../../../../sw/projects/bootloader/link.ld): the link script used to create the bootloader.
- [boxlambda_os/link.ld](../../../../sw/projects/boxlambda_os/link.ld): the link script used to create the BoxLambda OS application image . This link script is derived from `link_ddr_boot.ld`. It contains additional [sections and variables for the Forth subsystem](../../applications/boxlambda-os/forth/core.md#forth-linker-sections-and-variables) and the RAM disk region `fs_mem`.

