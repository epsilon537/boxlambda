\ BoxLambda Forth.
\
\ This may look like a Forth module but this not is a Forth module.
\ The syntax is limited to lines starting with \, which are ignored,
\ and lines starting with the word 'boxkern_include' followed by the full
\ path of a .fs module to be evaluated. That .fs module must not
\ include any submodules itself. The Word 'include' has not been defined yet
\ at this point.
\ The boxkern_include files are loaded and passed to Forth by the
\ BoxKern at boot time. It allows a limited form of Forth module loading until the
\ Forth 'include' Word can be defined.

\ The order is important. The modules build up a stack, with shell.fs on top.

boxkern_include forth/boxkern-includes/units.fs
boxkern_include forth/boxkern-includes/utils.fs
boxkern_include forth/boxkern-includes/range.fs
boxkern_include forth/boxkern-includes/array.fs
boxkern_include forth/boxkern-includes/except.fs
boxkern_include forth/boxkern-includes/lambda.fs
boxkern_include forth/boxkern-includes/struct.fs
boxkern_include forth/boxkern-includes/stack.fs
boxkern_include forth/boxkern-includes/compileto.fs
boxkern_include forth/boxkern-includes/heap.fs
boxkern_include forth/boxkern-includes/pool.fs
boxkern_include forth/boxkern-includes/temp-alloc.fs
boxkern_include forth/boxkern-includes/istr.fs
boxkern_include forth/boxkern-includes/escstr.fs
boxkern_include forth/boxkern-includes/tonumber.fs
boxkern_include forth/boxkern-includes/printf.fs
boxkern_include forth/boxkern-includes/cstr.fs
boxkern_include forth/boxkern-includes/fs.fs
boxkern_include forth/boxkern-includes/fs-redirect.fs
boxkern_include forth/boxkern-includes/shell.fs
boxkern_include forth/boxkern-includes/ifdef.fs
boxkern_include forth/boxkern-includes/prompt.fs
boxkern_include forth/boxkern-includes/disasm.fs
boxkern_include forth/boxkern-includes/dump.fs
boxkern_include forth/boxkern-includes/pre-dict.fs
boxkern_include forth/boxkern-includes/wordlist.fs
boxkern_include forth/boxkern-includes/dict.fs
boxkern_include forth/boxkern-includes/run-time-checking.fs
boxkern_include forth/boxkern-includes/module.fs
boxkern_include forth/boxkern-includes/bitfield.fs
boxkern_include forth/boxkern-includes/memmap.fs
boxkern_include forth/boxkern-includes/vera_regs.fs
boxkern_include forth/boxkern-includes/vera_spr_attr_ram.fs
boxkern_include forth/boxkern-includes/vera-palette.fs
boxkern_include forth/boxkern-includes/vec2.fs
boxkern_include forth/boxkern-includes/iter.fs
boxkern_include forth/boxkern-includes/fastboot.fs

