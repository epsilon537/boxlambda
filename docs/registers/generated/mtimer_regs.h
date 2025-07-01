// Created with Corsair v1.0.4
#ifndef __MTIMER_REGS_H
#define __MTIMER_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define MTIMER_BASE_ADDR 0x10020000

// MTIME - Machine-level time counter, low word.
#define MTIMER_MTIME_ADDR 0x0
#define MTIMER_MTIME_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // Machine-level time counter, low word.
  };
} mtimer_mtime_t;

// MTIME.VALUE - Machine-level time counter, low word.
#define MTIMER_MTIME_VALUE_WIDTH 32
#define MTIMER_MTIME_VALUE_LSB 0
#define MTIMER_MTIME_VALUE_MASK 0xffffffff
#define MTIMER_MTIME_VALUE_RESET 0x0

// MTIMEH - Machine-level time counter, high word.
#define MTIMER_MTIMEH_ADDR 0x4
#define MTIMER_MTIMEH_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // Machine-level time counter, high word.
  };
} mtimer_mtimeh_t;

// MTIMEH.VALUE - Machine-level time counter, high word.
#define MTIMER_MTIMEH_VALUE_WIDTH 32
#define MTIMER_MTIMEH_VALUE_LSB 0
#define MTIMER_MTIMEH_VALUE_MASK 0xffffffff
#define MTIMER_MTIMEH_VALUE_RESET 0x0

// MTIMECMP - Machine-level time compare, low word.
#define MTIMER_MTIMECMP_ADDR 0x8
#define MTIMER_MTIMECMP_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // Machine-level time compare, low word.
  };
} mtimer_mtimecmp_t;

// MTIMECMP.VALUE - Machine-level time compare, low word.
#define MTIMER_MTIMECMP_VALUE_WIDTH 32
#define MTIMER_MTIMECMP_VALUE_LSB 0
#define MTIMER_MTIMECMP_VALUE_MASK 0xffffffff
#define MTIMER_MTIMECMP_VALUE_RESET 0x0

// MTIMECMPH - Machine-level time compare, high word.
#define MTIMER_MTIMECMPH_ADDR 0xc
#define MTIMER_MTIMECMPH_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // Machine-level time compare, high word.
  };
} mtimer_mtimecmph_t;

// MTIMECMPH.VALUE - Machine-level time compare, high word.
#define MTIMER_MTIMECMPH_VALUE_WIDTH 32
#define MTIMER_MTIMECMPH_VALUE_LSB 0
#define MTIMER_MTIMECMPH_VALUE_MASK 0xffffffff
#define MTIMER_MTIMECMPH_VALUE_RESET 0x0

// MTIMEBLK - Blocking time compare register.
#define MTIMER_MTIMEBLK_ADDR 0x10
#define MTIMER_MTIMEBLK_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // A write operation to this register blocks the CPU until the lower 8 bits of the MTIME register match the written value.
    uint32_t : 24; // reserved
  };
} mtimer_mtimeblk_t;

// MTIMEBLK.VALUE - A write operation to this register blocks the CPU until the lower 8 bits of the MTIME register match the written value.
#define MTIMER_MTIMEBLK_VALUE_WIDTH 8
#define MTIMER_MTIMEBLK_VALUE_LSB 0
#define MTIMER_MTIMEBLK_VALUE_MASK 0xff
#define MTIMER_MTIMEBLK_VALUE_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t MTIME; // Machine-level time counter, low word.
        __IO mtimer_mtime_t MTIME_bf; // Bit access for MTIME register
    };
    union {
        __IO uint32_t MTIMEH; // Machine-level time counter, high word.
        __IO mtimer_mtimeh_t MTIMEH_bf; // Bit access for MTIMEH register
    };
    union {
        __IO uint32_t MTIMECMP; // Machine-level time compare, low word.
        __IO mtimer_mtimecmp_t MTIMECMP_bf; // Bit access for MTIMECMP register
    };
    union {
        __IO uint32_t MTIMECMPH; // Machine-level time compare, high word.
        __IO mtimer_mtimecmph_t MTIMECMPH_bf; // Bit access for MTIMECMPH register
    };
    union {
        __IO uint32_t MTIMEBLK; // Blocking time compare register.
        __IO mtimer_mtimeblk_t MTIMEBLK_bf; // Bit access for MTIMEBLK register
    };
} mtimer_t;

#define MTIMER ((mtimer_t*)(MTIMER_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __MTIMER_REGS_H */
