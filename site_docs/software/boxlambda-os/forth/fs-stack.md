# Forth Filesystem Stack

This diagram illustrates how the Forth modules listed in the [Word List](words.md) are stacked upon one another to attain Forth-level filesystem and shell access:

[![Forth Filesystem Stack.](../../../assets/fs-stack-layered.png)](../../../assets/fs-stack-layered.png)

*Forth Filesystem Stack.*

The Forth filesystem module `fs.fs` binds to FATFS via the [fs_ffi.cpp](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/fs_ffi.cpp)
FFI module. See [here](c-ffi.md) for more info on the Forth-C FFI.

