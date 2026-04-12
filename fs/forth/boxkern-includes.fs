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

boxkern_include forth/utils.fs
boxkern_include forth/range.fs
boxkern_include forth/units.fs
boxkern_include forth/array.fs
boxkern_include forth/except.fs
boxkern_include forth/lambda.fs
boxkern_include forth/struct.fs
boxkern_include forth/heap.fs
boxkern_include forth/pool.fs
boxkern_include forth/temp-alloc.fs
boxkern_include forth/istr.fs
boxkern_include forth/escstr.fs
boxkern_include forth/tonumber.fs
boxkern_include forth/printf.fs
boxkern_include forth/cstr.fs
boxkern_include forth/fs.fs
boxkern_include forth/fs-redirect.fs
boxkern_include forth/shell.fs

