/*
 * Epsilon, April 2024:
 * This version of crt0 is derived from picolibc's crt0 implementation
 * for RISCV.
 * The original file can be found here:
 *
 * https://github.com/picolibc/picolibc/blob/main/picocrt/machine/riscv/crt0.c
 *
 * This is the original license header:
 *
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * Copyright © 2020 Sebastian Meyer
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials provided
 *    with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "crt0.h"

/* BoxLambda: Early CRT0 shouldn't have libc dependencies because
 * libc is part of a yet-to-be-relocated linker section.
 * Instead, CRT0 relies on a simple local implementation of
 * memcpy and memset for section relocation and BSS clearing.
 */
static void local_memcpy(char *dst, char *src, unsigned num_bytes);
static void local_memset(char *dst, char val, unsigned num_bytes);

/* After the architecture-specific chip initialization is done, this
 * function initializes the data and bss segments.
 * This function is placed in the .init section. For SW images,
 * linked to boot from flash, this code will execute directly from
 * flash memory.*/
static void __attribute__((used)) __section(".init") _cstart(void) {
  /* BoxLambda: Copy emem code segment if needed.*/
  if (__ecode_start != __ecode_source) {
    local_memcpy(__ecode_start, __ecode_source, (uintptr_t)__ecode_size);
  }

  /*edata segment*/
  if (__edata_start != __edata_source) {
    local_memcpy(__edata_start, __edata_source, (uintptr_t)__edata_size);
  }

  /*eBSS segment*/
  local_memset(__ebss_start, '\0', (uintptr_t)__ebss_size);

  /* icode segment.*/
  if (__icode_start != __icode_source) {
    local_memcpy(__icode_start, __icode_source, (uintptr_t)__icode_size);
  }

  /*idata segment*/
  if (__idata_start != __idata_source) {
    local_memcpy(__idata_start, __idata_source, (uintptr_t)__idata_size);
  }

  /*iBSS segment*/
  local_memset(__ibss_start, '\0', (uintptr_t)__ibss_size);

/* BoxLambda: code and data has now been relocated, at this point we
 * can make non-local function calls. */
#ifdef PICOLIBC_TLS
  _set_tls(__tls_base);
#endif
#if defined(_HAVE_INITFINI_ARRAY) && CONSTRUCTORS
  __libc_init_array();
#endif

#ifdef CRT0_SEMIHOST
#define CMDLINE_LEN 1024
#define ARGV_LEN 64
  static char cmdline[CMDLINE_LEN];
  static char *argv[ARGV_LEN];
  int argc = 0;

  argv[argc++] = "program-name";
  if (sys_semihost_get_cmdline(cmdline, sizeof(cmdline)) == 0) {
    char *c = cmdline;

    while (*c && argc < ARGV_LEN - 1) {
      argv[argc++] = c;
      while (*c && *c != ' ')
        c++;
      if (!*c)
        break;
      *c = '\0';
      while (*++c == ' ')
        ;
    }
  }
  argv[argc] = NULL;
#else
#define argv NULL
#define argc 0
#endif

  int ret = main(argc, argv);
#ifdef CRT0_EXIT
  exit(ret);
#else
  (void)ret;
  for (;;)
    ;
#endif
}

/* BoxLambda:
 * Place these two functions in .init section so they execute
 * from flash memory (in case the SW image is linked to boot
 * from flash at least. For SW images linked to boot from IMEM,
 * the .init section is placed in IMEM).*/
static void __attribute__((used)) __section(".init")
    local_memcpy(char *dst, char *src, unsigned num_bytes) {
  char *end = dst + num_bytes;

  while (dst < end) {
    *dst++ = *src++;
  }
}

static void __attribute__((used)) __section(".init")
    local_memset(char *dst, char val, unsigned num_bytes) {
  char *end = dst + num_bytes;

  while (dst < end) {
    *dst++ = val;
  }
}

#ifdef CRT0_SEMIHOST
#include <semihost.h>
#include <stdio.h>
#include <unistd.h>

#ifdef __riscv_32e
#define NUM_REG 16
#else
#define NUM_REG 32
#endif

#if __riscv_xlen == 32
#define FMT "%08lx"
#define SD "sw"
#else
#define FMT "%016lx"
#define SD "sd"
#endif

struct fault {
  unsigned long r[NUM_REG];
  unsigned long mepc;
  unsigned long mcause;
  unsigned long mtval;
};

static const char *const names[NUM_REG] = {
    "zero",  "ra", "sp",  "gp",  "tp", "t0", "t1", "t2",
    "s0/fp", "s1", "a0",  "a1",  "a2", "a3", "a4", "a5",
#if NUM_REG > 16
    "a6",    "a7", "s2",  "s3",  "s4", "s5", "s6", "s7",
    "s8",    "s9", "s10", "s11", "t3", "t4", "t5", "t6",
#endif
};

static void __attribute__((used)) __section(".init")
    _ctrap(struct fault *fault) {
  int r;
  printf("RISCV fault\n");
  for (r = 0; r < NUM_REG; r++)
    printf("\tx%d %-5.5s%s 0x" FMT "\n", r, names[r], r < 10 ? " " : "",
           fault->r[r]);
  printf("\tmepc:     0x" FMT "\n", fault->mepc);
  printf("\tmcause:   0x" FMT "\n", fault->mcause);
  printf("\tmtval:    0x" FMT "\n", fault->mtval);
  _exit(1);
}

#define _PASTE(r) #r
#define PASTE(r) _PASTE(r)

void __attribute__((naked)) __section(".init") __attribute__((used))
__attribute((aligned(4))) _trap(void) {
#ifndef __clang__
  __asm__(".option	nopic");
#endif

  /* Build a known-working C environment */
  __asm__(".option	push\n"
          ".option	norelax\n"
          "la	sp, __stack\n"
          "la	gp, __global_pointer$\n"
          ".option	pop");

  /* Make space for saved registers */
  __asm__("addi   sp,sp,%0" ::"i"(-sizeof(struct fault)));

  /* Save registers on stack */
#define SAVE_REG(num)                                                          \
  __asm__(SD "     x%0, %1(sp)" ::"i"(num),                                    \
          "i"((num) * sizeof(unsigned long) + offsetof(struct fault, r)))

#define SAVE_REGS_8(base)                                                      \
  SAVE_REG(base + 0);                                                          \
  SAVE_REG(base + 1);                                                          \
  SAVE_REG(base + 2);                                                          \
  SAVE_REG(base + 3);                                                          \
  SAVE_REG(base + 4);                                                          \
  SAVE_REG(base + 5);                                                          \
  SAVE_REG(base + 6);                                                          \
  SAVE_REG(base + 7)

  SAVE_REGS_8(0);
  SAVE_REGS_8(8);
#ifndef __riscv_32e
  SAVE_REGS_8(16);
  SAVE_REGS_8(24);
#endif

#define SAVE_CSR(name)                                                         \
  __asm__("csrr   t0, " PASTE(name));                                          \
  __asm__(SD "  t0, %0(sp)" ::"i"(offsetof(struct fault, name)))

  SAVE_CSR(mepc);
  SAVE_CSR(mcause);
  SAVE_CSR(mtval);

  /*
   * Pass pointer to saved registers in first parameter register
   */
  __asm__("mv     a0, sp");

  /* Enable FPU (just in case) */
#ifdef __riscv_flen
  __asm__("csrr	t0, mstatus\n"
          "li	t1, 8192\n" // 1 << 13 = 8192
          "or	t0, t1, t0\n"
          "csrw	mstatus, t0\n"
          "csrwi	fcsr, 0");
#endif
  __asm__("j      _ctrap");
}
#endif

/* An application image starts with this header. */
unsigned const header_magic[2] __attribute__((section(".init.header"))) =
  {IMAGE_HEADER_MAGIC_NUMBER, (unsigned)__image_size};

void __attribute__((naked)) __section(".init.enter") __attribute__((used))
_start(void) {

  /**
   * seems clang has no option "nopic". Now this could be problematic,
   * since according to the clang devs at [0], that option has an effect
   * on `la`. However, the resulting crt0.o looks the same as the one from
   * gcc (same opcodes + pc relative relocations where I used `la`), so
   * this could be okay.
   * [0] https://reviews.llvm.org/D55325
   */
#ifndef __clang__
  __asm__(".option	nopic");
#endif

  __asm__(".option	push\n"
          ".option	norelax\n"
          "la	sp, __stack\n"
          "la	gp, __global_pointer$\n"
          ".option	pop");

  /*Set mtvec to 0, in imem (it might originally be at 0x11000000 in flash)*/
  __asm__("csrw	mtvec, x0");

#ifdef __riscv_flen
  __asm__("csrr	t0, mstatus\n"
          "li	t1, 8192\n" // 1 << 13 = 8192
          "or	t0, t1, t0\n"
          "csrw	mstatus, t0\n"
          "csrwi	fcsr, 0");
#endif
  __asm__("j      _cstart");
}
