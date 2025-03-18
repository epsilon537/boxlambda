---
hide:
  - toc
---

## The Flash Driver

**Flash Driver Component in the BoxLambda Directory Tree**:

[boxlambda/sw/components/flashdrvr](https://github.com/epsilon537/boxlambda/tree/master/sw/components/flashdrvr)

The primary purpose of the Spiflash Control Interface is to allow software to write to Flash. One does not simply write a byte to flash, however. The appropriate sector needs to be identified, erased, and paged (written). The **flashdrvr** software module implements this logic.

```
#define    FLASHCFG    0x100000C0
#define    FLASHBASE   0x11000000
#define    FLASHLEN    0x01000000

class    FLASHDRVR {
    ...
public:
    FLASHDRVR();
    ...
   /* Write the given block of data to the given flash memory address.
    * Note that flash memory is divided into 4KB segments. Writing to a segment
    * will destroy all previous contents of that segment.
    */
    bool    write(const unsigned addr, const unsigned len,
            const char *data, const bool verify=false);
    ...
};
```

