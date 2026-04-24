# Forth Filesystem Stack

This diagram shows how the Forth modules referenced in the [Word List](words.md) build on each other to reach Forth-level filesystem and shell
access:

[![Forth Filesystem Stack.](../../../assets/fs-stack-layered.png)](../../../assets/fs-stack-layered.png)

*Forth Filesystem Stack.*

The Forth filesystem module `fs.fs` binds to FATFS via the [fs_ffi.cpp](../../../../sw/components/forth/fs_ffi.cpp)
FFI module. See [here](c-ffi.md) for more info on the Forth-C FFI.
