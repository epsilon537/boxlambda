/*
 * Epsilon, April 2024:
 * This version of crt0 is derived from picolibc's crt0 implementation
 * for RISCV.
 * The original file can be found here:
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * https://github.com/picolibc/picolibc/blob/main/picocrt/machine/riscv/crt0.c
 *
 * This is the original license header:
 *
 * Copyright © 2019 Keith Packard
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

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <picotls.h>

extern char __icode_source[];
extern char __icode_start[];
extern char __icode_size[];
extern char __ecode_source[];
extern char __ecode_start[];
extern char __ecode_size[];
extern char __idata_source[];
extern char __idata_start[];
extern char __idata_end[];
extern char __idata_size[];
extern char __edata_source[];
extern char __edata_start[];
extern char __edata_end[];
extern char __edata_size[];
extern char __ibss_start[];
extern char __ibss_size[];
extern char __ebss_start[];
extern char __ebss_size[];
extern char __tls_base[];
extern char __tdata_end[];
extern char __tls_end[];
extern char __image_size[];
extern char __stack[];
extern char __stack_size[];
extern char __datastack[];
extern char __datastack_size[];
extern char __datastack_end[];

/* The first word in an application image has this magic number */
#define IMAGE_HEADER_MAGIC_NUMBER 0xB07A3BDA

/* These two functions must be defined in the architecture-specific
 * code
 */

void _start(void);

/* This is the application entry point */
int main(int, char **);

#ifdef _HAVE_INITFINI_ARRAY
extern void __libc_init_array(void);
#endif

#include <picotls.h>
#include <stdio.h>
#ifdef CRT0_SEMIHOST
#include <semihost.h>
#endif

#ifndef CONSTRUCTORS
#define CONSTRUCTORS 1
#endif

