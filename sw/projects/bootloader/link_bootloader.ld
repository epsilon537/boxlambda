/* This link script is derived from the picolibc link script. It creates a SW image that
 * boots from flash memory.
 */

/*
 * 'imem': Internal memory start address.
 */
__imem = 0x00000000;
__imem_size = 256k; /*256KB internal memory*/

/* stack:*/
__stack_size = 512;

/*'emem' is the external memory section.*/
__emem = 0x20000000;
__emem_size = 256m; /*256MB external memory.*/
__heap_size = 128m;

/*'fmem' is the flash memory section reserved for the bootloader.*/
__fmem = 0x11400000;
__fmem_size = 1m; /*1MB flash memory.*/

ENTRY(_start)

MEMORY
{
    imem : ORIGIN = __imem, LENGTH = __imem_size
    emem : ORIGIN = __emem, LENGTH = __emem_size
    fmem : ORIGIN = __fmem, LENGTH = __fmem_size
}

SECTIONS
{
    /*Place the stack at the end of imem.*/
    PROVIDE(__stack = ORIGIN(imem) + LENGTH(imem));


    .ftext : {
        KEEP (*(.vectors))
        KEEP (*(.text.init.enter))
        KEEP (*(.data.init.enter))
        KEEP (*(SORT_BY_NAME(.init) SORT_BY_NAME(.init.*)))
        *(.text.startup .text.startup.*)

        . = ALIGN(8);
    } >fmem

    .itext : {
        KEEP (*(.vectors))
        *(.init.enter)
        *(.init)
        *(.text .text.*)
        *(.gnu.linkonce.t.*)
        KEEP (*(.fini .fini.*))

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
    } >imem AT>fmem

    PROVIDE( __icode_end = . );
    PROVIDE( __icode_start = ADDR(.itext) );
    PROVIDE( __icode_source = LOADADDR(.itext) );
    PROVIDE( __icode_size = __icode_end - __icode_start );

    .etext : {
        *(.etext .etext.*)

        . = ALIGN(8);
    } >emem AT>fmem

    PROVIDE( __ecode_end = . );
    PROVIDE( __ecode_start = ADDR(.etext) );
    PROVIDE( __ecode_source = LOADADDR(.etext) );
    PROVIDE( __ecode_size = __ecode_end - __ecode_start );

    .edata : ALIGN_WITH_INPUT {
        *(.edata .edata.*)

        . = ALIGN(8);
    } >emem AT>fmem

    PROVIDE( __edata_end = . );
    PROVIDE( __edata_start = ADDR(.edata) );
    PROVIDE( __edata_source = LOADADDR(.edata) );
    PROVIDE( __edata_size = __edata_end - __edata_start );

   /*
    * Data values which are preserved across reset
    */
    .preserve (NOLOAD) : {
        PROVIDE(__preserve_start__ = .);
        KEEP(*(SORT_BY_NAME(.preserve.*)))
        KEEP(*(.preserve))
        PROVIDE(__preserve_end__ = .);
    } >imem AT>fmem

    .idata : ALIGN_WITH_INPUT {
        *(.data .data.*)
        *(.gnu.linkonce.d.*)

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

    } >imem AT>fmem

   /* Thread local initialized data.*/
    .tdata : ALIGN_WITH_INPUT {
        *(.tdata .tdata.* .gnu.linkonce.td.*)
    } >imem AT>fmem

    PROVIDE( __tdata_end = . );
    PROVIDE( __tdata_start = ADDR(.tdata) );
    PROVIDE( __tdata_source = LOADADDR(.tdata) );
    PROVIDE( __tdata_size = SIZEOF(.tdata) );
    PROVIDE( __tls_base = ADDR(.tdata));

    PROVIDE( __idata_end = . );
    PROVIDE( __idata_start = ADDR(.idata) );
    PROVIDE( __idata_source = LOADADDR(.idata) );
    PROVIDE( __idata_size = __idata_end - __idata_start );

    .tbss (NOLOAD) : {
        *(.tbss .tbss.* .gnu.linkonce.tb.*)
        *(.tcommon)
    } >imem

    PROVIDE( __tls_end = . );
    PROVIDE( __tbss_end = . );
    PROVIDE( __ibss_start = ADDR(.tbss));
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
    } >imem

    .ibss (NOLOAD) : {
        *(.sbss*)
        *(.gnu.linkonce.sb.*)
        *(.bss .bss.*)
        *(.gnu.linkonce.b.*)
        *(COMMON)
    } >imem

    PROVIDE( __ibss_end = . );
    PROVIDE( __ibss_size = __ibss_end - __ibss_start );

    .ebss (NOLOAD) : {
        *(.ebss .ebss.*)
    } >emem

    PROVIDE( __ebss_end = . );
    PROVIDE( __ebss_start = ADDR(.ebss) );
    PROVIDE( __ebss_size = __ebss_end - __ebss_start );

    /*Put the heap in emem*/
    .heap (NOLOAD) : {
        . += (__heap_size);
        __heap_end = .;
    } >emem

    PROVIDE (__heap_start = ADDR(.heap));

    /* crt0 expects this but value is not relevant for bootloader.*/
    PROVIDE (__image_size = 0);

    /* Throw away C++ exception handling information */

    /DISCARD/ : {
        *(.note .note.*)
        *(.eh_fdmeme .eh_fdmeme.*)
        *(.ARM.extab* .gnu.linkonce.armextab.*)
        *(.ARM.exidx*)
    }
}

