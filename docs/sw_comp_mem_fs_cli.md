---
hide:
  - toc
---

# Memory and File System CLI

- **Mem_fs_cli CLI Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/mem_fs_cli](https://github.com/epsilon537/boxlambda/tree/master/sw/components/mem_fs_cli)

This component provides CLI access to memory management and file system functions.

```
* rm
    rm <filename> : Remove (delete) file.
* save
    save <filename> <address> <size in bytes> : write memory contents to file.
* load
    load <filename> [address] : read file into memory. Alloc mem. buf. if addr. not given.
* allocBuf
    allocBuf <size> : allocate from heap buffer of given size.
* relBuf
    relBuf <address> : release the buffer at given adress.
* ls
    list directory contents.
```

