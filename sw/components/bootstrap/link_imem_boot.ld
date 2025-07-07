/* This link script is derived from the picolibc link script. It creates a SW image that
 * boots from IMEM internal memory.
 * In this type of image, all load sections are placed in IMEM.
 *
 * The link script (starting after the ENTRY directive below) expects the following symbols
 * to be set:
 *
 * 'imem': Internal memory start address.
 */
__imem = 0x00000000;
__imem_size = 256k; /*256KB internal memory*/

/* stack:*/
__stack_size = 512;

/*'emem' is the external memory section. The heap will be placed here.*/
__emem = 0x20000000;
__emem_size = 256m; /*256MB external memory.*/
__heap_size = 128m; /*of which 128MB is used as heap currently.*/

ENTRY(_start)

MEMORY
{
    imem : ORIGIN = __imem, LENGTH = __imem_size
    emem : ORIGIN = __emem, LENGTH = __emem_size
}

SECTIONS
{
    /*Place the stack at the end of imem.*/
    PROVIDE(__stack = ORIGIN(imem) + LENGTH(imem));

    .text : {
        /*In this link map, code start and code source are the same.
         *As a result, the code section does not get relocated by crt0.*/
        PROVIDE(__code_start = .);
        PROVIDE(__code_source = .);

        KEEP (*(.vectors)) /*add vectors to the start of the .init section.*/
        KEEP (*(.text.init.enter))
        KEEP (*(.data.init.enter))
        KEEP (*(SORT_BY_NAME(.init) SORT_BY_NAME(.init.*)))
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
    } >imem

    PROVIDE( __code_size = __code_end - __code_start );

   /*
    * Data values which are preserved across reset
    */
    .preserve (NOLOAD) : {
        PROVIDE(__preserve_start__ = .);
        KEEP(*(SORT_BY_NAME(.preserve.*)))
        KEEP(*(.preserve))
        PROVIDE(__preserve_end__ = .);
    } >imem

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

    } >imem

    PROVIDE(__data_start = ADDR(.data));
    PROVIDE(__data_source = LOADADDR(.data));

   /* Thread local initialized data.*/
    .tdata : ALIGN_WITH_INPUT {
        *(.tdata .tdata.* .gnu.linkonce.td.*)
        /*Notice that __data_end is set here.*/
        PROVIDE(__data_end = .);
        PROVIDE(__tdata_end = .);
    } >imem

    PROVIDE( __tls_base = ADDR(.tdata));
    PROVIDE( __tdata_start = ADDR(.tdata));
    PROVIDE( __tdata_source = LOADADDR(.tdata) );
    PROVIDE( __tdata_source_end = LOADADDR(.tdata) + SIZEOF(.tdata) );
    PROVIDE( __tdata_size = SIZEOF(.tdata) );

    /*edata marks the end of the initialized data section*/
    PROVIDE( __edata = __data_end );
    PROVIDE( _edata = __data_end );
    PROVIDE( edata = __data_end );
    PROVIDE( __data_size = __data_end - __data_start );

    .tbss (NOLOAD) : {
        *(.tbss .tbss.* .gnu.linkonce.tb.*)
        *(.tcommon)
        PROVIDE( __tls_end = . );
        PROVIDE( __tbss_end = . );
    } >imem

    /*Notice that __bss_start is set here*/
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
    } >imem

    .bss (NOLOAD) : {
        *(.sbss*)
        *(.gnu.linkonce.sb.*)
        *(.bss .bss.*)
        *(.gnu.linkonce.b.*)
        *(COMMON)
        __bss_end = .;
    } >imem

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
    } >imem

    /* Throw away C++ exception handling information */

    /DISCARD/ : {
        *(.note .note.*)
        *(.eh_fdmeme .eh_fdmeme.*)
        *(.ARM.extab* .gnu.linkonce.armextab.*)
        *(.ARM.exidx*)
    }
}

