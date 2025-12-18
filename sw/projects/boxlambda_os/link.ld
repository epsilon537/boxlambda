/* This link script is derived from the picolibc link script. It creates a SW image that
 * boots from flash memory.
 */

/*
 * 'imem': Internal memory start address.
 */
__imem = 0x00000000;
__imem_size = 256k; /*256KB internal memory*/

/* (return) stack:*/
PROVIDE(__stack_size = 1024);

/* data stack:*/
PROVIDE(__datastack_size = 1024);

/*'emem' is the external memory section.*/
__emem = 0x20000000;
__emem_size = 256m; /*256MB external memory.*/
__heap_size = 128m;

/*'fmem' is the flash memory section.*/
__fmem = 0x11600000;
__fmem_size = 2m; /*2MB flash memory available for the SW image.*/

/*Forth IMEM size*/
__forth_imem_size = 92k;

/*Forth external memory size*/
__forth_emem_size = 1m;

ENTRY(_start)

MEMORY
{
    imem : ORIGIN = __imem, LENGTH = __imem_size
    emem : ORIGIN = __emem, LENGTH = __emem_size
    fmem : ORIGIN = __fmem, LENGTH = __fmem_size
}

SECTIONS
{
    /* Place the (return) stack pointer at the end of imem.*/
    PROVIDE(__stack = ORIGIN(imem) + LENGTH(imem));
    /* Put the data stack in front of it.*/
    PROVIDE(__datastack = __stack - __stack_size);
    PROVIDE(__datastack_end = __datastack - __datastack_size);

    .etext : {
        KEEP (*(.init.header))
        *(.init.enter)
        *(.init)
        *(.text.unlikely .text.unlikely.*)
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
    } >emem

    PROVIDE( __ecode_end = . );
    PROVIDE( __ecode_start = ADDR(.etext) );
    PROVIDE( __ecode_source = LOADADDR(.etext) );
    PROVIDE( __ecode_size = __ecode_end - __ecode_start );

    .itext : {
        KEEP (*(.vectors))
        *(.itext .itext.*)

        *(.forth_core)

        . = ALIGN(8);
    } >imem AT>emem

    PROVIDE( __icode_end = . );
    PROVIDE( __icode_start = ADDR(.itext) );
    PROVIDE( __icode_source = LOADADDR(.itext) );
    PROVIDE( __icode_size = __icode_end - __icode_start );

    .idata : ALIGN_WITH_INPUT {
        *(.idata .idata.*)

        . = ALIGN(8);
    } >imem AT>emem

    PROVIDE( __idata_end = . );
    PROVIDE( __idata_start = ADDR(.idata) );
    PROVIDE( __idata_source = LOADADDR(.idata) );
    PROVIDE( __idata_size = __idata_end - __idata_start );

   /*
    * Data values which are preserved across reset
    */
    .preserve (NOLOAD) : {
        PROVIDE(__preserve_start__ = .);
        KEEP(*(SORT_BY_NAME(.preserve.*)))
        KEEP(*(.preserve))
        PROVIDE(__preserve_end__ = .);
    } >emem

    .edata : ALIGN_WITH_INPUT {
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
    } >emem

   /* Thread local initialized data.*/
    .tdata : ALIGN_WITH_INPUT {
        *(.tdata .tdata.* .gnu.linkonce.td.*)
    } >emem

    PROVIDE( __tdata_end = . );
    PROVIDE( __tdata_start = ADDR(.tdata) );
    PROVIDE( __tdata_source = LOADADDR(.tdata) );
    PROVIDE( __tdata_size = SIZEOF(.tdata) );
    PROVIDE( __tls_base = ADDR(.tdata));
    PROVIDE( __image_size = __tdata_source + __tdata_size - __ecode_source );

    PROVIDE( __edata_end = . );
    PROVIDE( __edata_start = ADDR(.edata) );
    PROVIDE( __edata_source = LOADADDR(.edata) );
    PROVIDE( __edata_size = __edata_end - __edata_start );

    .tbss (NOLOAD) : {
        *(.tbss .tbss.* .gnu.linkonce.tb.*)
        *(.tcommon)
    } >emem

    PROVIDE( __tls_end = . );
    PROVIDE( __tbss_end = . );
    PROVIDE( __tbss_start = ADDR(.tbss));
    PROVIDE( __tbss_size = SIZEOF(.tbss) );
    PROVIDE( __tls_size = __tls_end - __tls_base );

   /*
    * The linker special cases .tbss segments which are
    * identified as segments which are not loaded and are
    * thread_local.
    *
    * For .tbss specifically, the linker does not advance 'dot'
    * across them.  We actually need memory allocated for tbss,
    * so we create a special segment here just to make room
    */
    .tbss_space (NOLOAD) : {
        . = . + SIZEOF(.tbss);
    } >emem

    .ebss (NOLOAD) : {
        *(.sbss*)
        *(.gnu.linkonce.sb.*)
        *(.bss .bss.*)
        *(.gnu.linkonce.b.*)
        *(COMMON)
    } >emem

    PROVIDE( __ebss_start = ADDR(.tbss) );
    PROVIDE( __ebss_end = . );
    PROVIDE( __ebss_size = SIZEOF(.ebss) );

    .forth_imem (NOLOAD) : {
        . += __forth_imem_size;
    } >imem

    PROVIDE( __forth_imem_end = . );
    PROVIDE( __forth_imem_start = ADDR(.forth_imem) );
    PROVIDE( __forth_imem_size = SIZEOF(.forth_imem) );

    .ibss (NOLOAD) : {
        *(.ibss .ibss.*)
        /* Check that the location pointer doesn't cross into the stack region*/
        ASSERT(. < __datastack - __datastack_size, "IBSS doesn't fit in IMEM.");
    } >imem

    PROVIDE( __ibss_end = . );
    PROVIDE( __ibss_start = ADDR(.ibss) );
    PROVIDE( __ibss_size = SIZEOF(.ibss) );

    .forth_emem (NOLOAD) : {
        . += __forth_emem_size;
    } >emem

    PROVIDE( __forth_emem_end = . );
    PROVIDE( __forth_emem_start = ADDR(.forth_emem) );
    PROVIDE( __forth_emem_size = SIZEOF(.forth_emem) );

    /*Put the heap in emem*/
    .heap (NOLOAD) : {
        . += (__heap_size);
        __heap_end = .;
    } >emem

    PROVIDE (__heap_start = ADDR(.heap));


    /* Throw away C++ exception handling information */

    /DISCARD/ : {
        *(.note .note.*)
        *(.eh_fdmeme .eh_fdmeme.*)
        *(.ARM.extab* .gnu.linkonce.armextab.*)
        *(.ARM.exidx*)
    }
}

