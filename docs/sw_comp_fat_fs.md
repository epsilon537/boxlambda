---
hide:
  - toc
---

## FatFS

- **FatFs Repo**, BoxLambda fork, *boxlambda* branch:
    [https://github.com/epsilon537/fatfs/tree/boxlambda](https://github.com/epsilon537/fatfs/tree/boxlambda)

- **FatFs Submodule in the BoxLambda Directory Tree**:
    boxlambda/sub/fatfs/.

- **FatFs Website**:
    [http://elm-chan.org/fsw/ff/00index_e.html](http://elm-chan.org/fsw/ff/00index_e.html)

- **FatFs Software Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/fatfs](https://github.com/epsilon537/boxlambda/tree/master/sw/components/fatfs)

[FatFs](http://elm-chan.org/fsw/ff/00index_e.html) is a lightweight software library for small systems that implements FAT file system support. It's written in ANSI C89 and has no dependencies other than a minimal C environment. It'll compile out of the box in virtually any environment.

FatFs itself does not provide the device/media-specific _Storage Device_ Controls*. Those have to come from the device implementer. The SDSPI submodule provides these functions for FatFs. Four files are provided: [sdcard.c, sdcard.h, diskio.c, and board.h](https://github.com/epsilon537/sdspi/tree/boxlambda/sw).

![FatFs Media Access Interface.](assets/FatFs_Media_Access_Interface.drawio.png)

*FatFs Media Access Interface.*

### FatFs Configuration

FatFs is very configurable, so you can trade options for footprint.
All configuration options are well-documented and centralized in the *ffconf.h* file.
Relative to the default settings, I modified the following:

- **Enable FF_USE_FIND**: filtered directory read functions, *f_findfirst()* and *f_findnext()*.
- **Enable FF_USE_STRFUNC**: string functions, *f_gets()*, *f_putc()*, *f_puts()*, and *f_printf()*.
- **Enable FF_FS_RPATH**: support for relative paths.
- **Enable FF_FS_NORTC**: I *disabled* the timestamp feature. I will revisit this when I bring up RTC on BoxLambda.

[https://github.com/epsilon537/fatfs/blob/boxlambda/source/ffconf.h](https://github.com/epsilon537/fatfs/blob/boxlambda/source/ffconf.h)

### FatFs_Test

FatFs itself does not provide a test suite, but I found a simple test sequence in [another project](https://github.com/avrxml/asf/blob/master/thirdparty/fatfs/unit_tests/unit_tests.c). I used that code as the starting point for a BoxLambda [fatfs_test](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/fatfs_test/fatfs_test.c).

Memory Footprint
----------------

|                        | Code (KB) | RO-Data (KB) | RW-Data (KB) |
| ---------------------- | ----------| ------------ | ------------ |
| FatFs                  | 14.1      | 0.8          | 0.3          |
| Stack                  | 0         | 0            | 0.5          |

