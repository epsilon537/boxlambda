/* This link script is derived from the picolibc link script. It creates a SW image that
 * boots from flash memory.
 * In this type of image, all load sections are placed in flash memory. From there, the code
 * sections are relocated to CMEM. The data sections are relocated to DMEM.
 *
 * The link script (starting after the ENTRY directive below) expects the following symbols
 * to be set:
 *
 * 'cmem' or 'code memory' contains code. You can also put BSS data in
 * cmem by assigning it to the .cmem_bss section. I did not (yet) add a .cmem_data section.
 */
__cmem = 0x00000000;
/*__cmem_size is no longer specified here. It is now passed in as a linker option*/

/* 'dmem' or 'data memory' contains data segments, bss and stack. You can put code in dmem
 * by assigning it
 * to the .dmem_text section.*/
__dmem = 0x00020000;
/*__dmem_size is no longer specified here. It is now passed in as a linker option*/

/* stack:*/
__stack_size = 512;

/*'emem' the the external memory section. The heap will be placed here.*/
__emem = 0x20000000;
__emem_size = 256m; /*256MB external memory.*/
__heap_size = 128m; /*of which 128MB is used as heap currently.*/

/* flash memory section:*/
__flash = 0x11400000; /*0x1100000 is base of flash memory + 4MB offset for the bitstream*/
/* Allow 4MB of flash memory, i.e. the range 0x11400000-0x11800000, for software images.
 * This still leaves 0x11800000 0x12000000 (8mb) available for other non-volatile data storage.*/
__flash_size = 4m;

ENTRY(_start)

MEMORY
{
    flash : ORIGIN = __flash, LENGTH = __flash_size
    cmem : ORIGIN = __cmem, LENGTH = __cmem_size
    dmem : ORIGIN = __dmem, LENGTH = __dmem_size
    emem : ORIGIN = __emem, LENGTH = __emem_size
}

SECTIONS
{
    /*Place the stack at the end of dmem.*/
    PROVIDE(__stack = ORIGIN(dmem) + LENGTH(dmem));

    .flash : {
        KEEP (*(.text.init.enter))
        KEEP (*(SORT_BY_NAME(.init) SORT_BY_NAME(.init.*)))
        KEEP (*(.data.init.enter))
    } >flash

    .text : {
        PROVIDE(__code_start = ADDR(.text));
        KEEP (*(.vectors)) /*add vectors to the start of CMEM.*/
        /* code */
        *(.text.unlikely .text.unlikely.*)
        *(.text.startup .text.startup.*)
        *(.text .text.*)
        *(.gnu.linkonce.t.*)
        KEEP (*(.fini .fini.*))
        __text_end = .;

        PROVIDE (__etext = __text_end);
        PROVIDE (_etext = __text_end);
        PROVIDE (etext = __text_end);

        /* Need to pre-align so that the symbols come after padding */
        . = ALIGN(8);

        /* lists of constructors and destructors */
        PROVIDE_HIDDEN ( __preinit_array_start = . );
        KEEP (*(.preinit_array))
        PROVIDE_HIDDEN ( __preinit_array_end = . );

        PROVIDE_HIDDEN ( __init_array_start = . );
        KEEP (*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.ctors.*)))
        KEEP (*(.init_array .ctors))
        PROVIDE_HIDDEN ( __init_array_end = . );

        PROVIDE_HIDDEN ( __fini_array_start = . );
        KEEP (*(SORT_BY_INIT_PRIORITY(.fini_array.*) SORT_BY_INIT_PRIORITY(.dtors.*)))
        KEEP (*(.fini_array .dtors))
        PROVIDE_HIDDEN ( __fini_array_end = . );

        PROVIDE(__code_end = .);
    } >cmem AT>flash
    PROVIDE(__code_source = LOADADDR(.text));
    PROVIDE(__code_size = __code_end - __code_start );

   /*BSS data can be mapped to cmem by assigning it to this segment.*/
    .cmem_bss (NOLOAD) : {
        *(.cmem_bss)
        __cmem_bss_end = .;
    } >cmem
    PROVIDE(__cmem_bss_start = ADDR(.cmem_bss));
    PROVIDE(__cmem_bss_size = __cmem_bss_end - __cmem_bss_start );

   /*
    * Data values which are preserved across reset
    */
    .preserve (NOLOAD) : {
        PROVIDE(__preserve_start__ = .);
        KEEP(*(SORT_BY_NAME(.preserve.*)))
        KEEP(*(.preserve))
        PROVIDE(__preserve_end__ = .);
    } >dmem

    .data : ALIGN_WITH_INPUT {
        *(.data .data.*)
        *(.gnu.linkonce.d.*)

        /* Need to pre-align so that the symbols come after padding */
        . = ALIGN(8);

        PROVIDE( __global_pointer$ = . + 0x800 );
        /* read-only data */
        *(.rdata)
        *(.rodata .rodata.*)
        *(.gnu.linkonce.r.*)

        *(.srodata.cst16)
        *(.srodata.cst8)
        *(.srodata.cst4)
        *(.srodata.cst2)
        *(.srodata .srodata.*)
        *(.data.rel.ro .data.rel.ro.*)
        *(.got .got.*)

        *(.sdata .sdata.* .sdata2.*)
        *(.gnu.linkonce.s.*)

        /*Code can be mapped to DMEM by assigning it to this segment.*/
        *(.dmem_text)
    } >dmem AT>flash

    PROVIDE(__data_start = ADDR(.data));
    PROVIDE(__data_source = LOADADDR(.data));

   /* Thread local initialized data. This gets
    * space allocated as it is expected to be placed
    * in dmem to be used as a template for TLS data blocks
    * allocated at runtime. We're slightly abusing that
    * by placing the data in cmem where it will be copied
    * into the allocate dmem addresses by the existing
    * data initialization code in crt0
    */
    .tdata : ALIGN_WITH_INPUT {
        *(.tdata .tdata.* .gnu.linkonce.td.*)
        PROVIDE(__data_end = .);
        PROVIDE(__tdata_end = .);
    } >dmem AT>flash

    PROVIDE( __tls_base = ADDR(.tdata));
    PROVIDE( __tdata_start = ADDR(.tdata));
    PROVIDE( __tdata_source = LOADADDR(.tdata) );
    PROVIDE( __tdata_source_end = LOADADDR(.tdata) + SIZEOF(.tdata) );
    PROVIDE( __tdata_size = SIZEOF(.tdata) );

    PROVIDE( __edata = __data_end );
    PROVIDE( _edata = __data_end );
    PROVIDE( edata = __data_end );
    PROVIDE( __data_size = __data_end - __data_start );

    .tbss (NOLOAD) : {
        *(.tbss .tbss.* .gnu.linkonce.tb.*)
        *(.tcommon)
        PROVIDE( __tls_end = . );
        PROVIDE( __tbss_end = . );
    } >dmem

    PROVIDE( __bss_start = ADDR(.tbss));
    PROVIDE( __tbss_start = ADDR(.tbss));
    PROVIDE( __tbss_size = SIZEOF(.tbss) );
    PROVIDE( __tls_size = __tls_end - __tls_base );

   /*
    * The linker special cases .tbss segments which are
    * identified as segments which are not loaded and are
    * thread_local.
    *
    * For these segments, the linker does not advance 'dot'
    * across them.  We actually need memory allocated for tbss,
    * so we create a special segment here just to make room
    */
    .tbss_space (NOLOAD) : {
        . = . + SIZEOF(.tbss);
    } >dmem

    .bss (NOLOAD) : {
        *(.sbss*)
        *(.gnu.linkonce.sb.*)
        *(.bss .bss.*)
        *(.gnu.linkonce.b.*)
        *(COMMON)
        __bss_end = .;
    } >dmem

    PROVIDE( __end = __bss_end );
    PROVIDE( _end = __bss_end );
    PROVIDE( end = __bss_end );
    PROVIDE( __bss_size = __bss_end - __bss_start );

    /*Put the heap in emem*/
    .heap (NOLOAD) : {
        . += (__heap_size);
        __heap_end = .;
    } >emem

    PROVIDE (__heap_start = ADDR(.heap));

    /* Define a stack region to make sure it fits in memory */
    .stack (NOLOAD) : {
       . += __stack_size;
    } >dmem

    /* Throw away C++ exception handling information */

    /DISCARD/ : {
        *(.note .note.*)
        *(.eh_fdmeme .eh_fdmeme.*)
        *(.ARM.extab* .gnu.linkonce.armextab.*)
        *(.ARM.exidx*)
    }
}

