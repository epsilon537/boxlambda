/*The picolibc link script (starting after the ENTRY directive below) expects these symbols to be set.*/
__flash = 0x00000000; //'flash' is the read-only section of the image, containing code and read-only data
__flash_size = 32k;
__ram = 0x00008000;   //'ram' is the read-write section of the image, containing data segments, bss and stack
__ram_size = 32k;
__stack_size = 512;
/*In other words: We have 64K of memory. We divide it into a 32K read-only section and a 32K read-write section. */
/*There's not really good reason to split our internal memory in two this way, but this is how the picolibc link script does things. */
/*I think it's best to just go with it for the time being, at least for picolibc bring-up.*/

/*What follows is based on picolibc.ld, with modifications to hook in vectors.*/

ENTRY(_start)

/*
* These values should be provided by the application. We'll include
* some phony values here to make things link for testing
*/

MEMORY
{
flash (rxai!w) : ORIGIN = DEFINED(__flash) ? __flash : 0x10000000, LENGTH = DEFINED(__flash_size) ? __flash_size : 0x10000
ram (wxa!ri)   : ORIGIN = DEFINED(__ram  ) ? __ram   : 0x20000000, LENGTH = DEFINED(__ram_size  ) ? __ram_size   : 0x08000
}

PHDRS
{
text PT_LOAD;
ram PT_LOAD;
ram_init PT_LOAD;
tls PT_TLS;
}

SECTIONS
{
PROVIDE(__stack = ORIGIN(ram) + LENGTH(ram));

.init : {
KEEP (*(.vectors)) /*add vectors to the start of the .init section.*/
KEEP (*(.text.init.enter))
KEEP (*(.data.init.enter))
KEEP (*(SORT_BY_NAME(.init) SORT_BY_NAME(.init.*)))
} >flash AT>flash :text

.text : {

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
} >flash AT>flash :text

/* additional sections when compiling with C++ exception support */
/*
.except_ordered : {
*(.gcc_except_table *.gcc_except_table.*)
KEEP (*(.eh_frame .eh_frame.*))
*(.ARM.extab* .gnu.linkonce.armextab.*)
} >flash AT>flash :text

.except_unordered : {
. = ALIGN(8);

PROVIDE(__exidx_start = .);
*(.ARM.exidx*)
PROVIDE(__exidx_end = .);
} >flash AT>flash :text
*/

/*
* Data values which are preserved across reset
*/
.preserve (NOLOAD) : {
PROVIDE(__preserve_start__ = .);
KEEP(*(SORT_BY_NAME(.preserve.*)))
KEEP(*(.preserve))
PROVIDE(__preserve_end__ = .);
} >ram AT>ram :ram

.data : ALIGN_WITH_INPUT {
*(.data .data.*)
*(.gnu.linkonce.d.*)

/* Need to pre-align so that the symbols come after padding */
. = ALIGN(8);

PROVIDE( __global_pointer$ = . + 0x800 );
*(.sdata .sdata.* .sdata2.*)
*(.gnu.linkonce.s.*)
} >ram AT>flash :ram_init
PROVIDE(__data_start = ADDR(.data));
PROVIDE(__data_source = LOADADDR(.data));

/* Thread local initialized data. This gets
* space allocated as it is expected to be placed
* in ram to be used as a template for TLS data blocks
* allocated at runtime. We're slightly abusing that
* by placing the data in flash where it will be copied
* into the allocate ram addresses by the existing
* data initialization code in crt0
*/
.tdata : ALIGN_WITH_INPUT {
*(.tdata .tdata.* .gnu.linkonce.td.*)
PROVIDE(__data_end = .);
PROVIDE(__tdata_end = .);
} >ram AT>flash :tls :ram_init
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
} >ram AT>ram :tls :ram
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
} >ram AT>ram :ram

.bss (NOLOAD) : {
*(.sbss*)
*(.gnu.linkonce.sb.*)
*(.bss .bss.*)
*(.gnu.linkonce.b.*)
*(COMMON)

/* Align the heap */
. = ALIGN(8);
__bss_end = .;
} >ram AT>ram :ram
PROVIDE( __end = __bss_end );
PROVIDE( _end = __bss_end );
PROVIDE( end = __bss_end );
PROVIDE( __bss_size = __bss_end - __bss_start );

/* Make the rest of memory available for heap storage */
PROVIDE (__heap_start = __end);
PROVIDE (__heap_end = __stack - (DEFINED(__stack_size) ? __stack_size : 0x800));
PROVIDE (__heap_size = __heap_end - __heap_start);

/* Define a stack region to make sure it fits in memory */
.stack (NOLOAD) : {
. += (DEFINED(__stack_size) ? __stack_size : 0x800);
} >ram :ram

/* Throw away C++ exception handling information */



/DISCARD/ : {
*(.note .note.*)
*(.eh_frame .eh_frame.*)
*(.ARM.extab* .gnu.linkonce.armextab.*)
*(.ARM.exidx*)
}


}
