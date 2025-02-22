#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "i2c_regs.h"
#include "vera_hal.h"

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

#define BUF_NUM_WORDS 4096

#define NUM_ITERATIONS 100

static struct uart uart0;
static struct gpio gpio;

static uint32_t srcBuf[BUF_NUM_WORDS];
static uint32_t dstBuf[BUF_NUM_WORDS];

uint32_t do_nothing() {
  mcycle_stop();
  pcount_reset();
  mcycle_start();
  mcycle_stop();

  uint32_t cycles = mcycle_get32();

  printf("Do nothing: %d cycles.\n", cycles);

  return cycles;
}

//Repeatedly read and write a SoC register and measure how long it takes.
uint32_t lw_register_loop(void *reg) {
  uint32_t num_iterations = NUM_ITERATIONS;

  mcycle_stop();
  pcount_reset();
  mcycle_start();

    __asm__ volatile (
        "2: \n"
        "lw t0, (%0) \n"   // Load word from register
        "addi %1, %1, -1 \n" // Decrement counter
        "bnez %1, 2b \n"   // Loop if counter != 0
        : "+r"(reg), "+r"(num_iterations)
        :
        : "t0", "memory"
    );

  mcycle_stop();

  uint32_t cycles = mcycle_get32()/NUM_ITERATIONS;

  printf("lw_sw_register_loop: addr: 0x%x, %d cycles/iteration.\n", (uint32_t)reg, cycles);

  return cycles;
}

uint32_t lw_sw_copy_loop(void *dest, const void *src) {
  uint32_t num_iterations = NUM_ITERATIONS;

  mcycle_stop();
  pcount_reset();
  mcycle_start();

    __asm__ volatile (
        "1: \n"
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "addi %2, %2, -1 \n" // Decrement counter
        "bnez %2, 1b \n"   // Loop if counter != 0
        : "+r"(dest), "+r"(src), "+r"(num_iterations)
        :
        : "t0", "memory"
    );

  mcycle_stop();

  uint32_t cycles = mcycle_get32()/100;

  printf("lw_sw_copy_loop: dest: 0x%x, src: 0x%x, %d cycles/iteration.\n", (uint32_t)dest, (uint32_t)src, cycles);

  return cycles;
}

//100 iterations of the lw_sw_copy_loop unrolled
uint32_t lw_sw_copy_unrolled(void *dest, const void *src) {
  mcycle_stop();
  pcount_reset();
  mcycle_start();

    __asm__ volatile (
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        "lw t0, (%1) \n"   // Load word from source
        "sw t0, (%0) \n"   // Store word to destination
        "addi %0, %0, 4 \n" // Increment destination pointer (4 bytes)
        "addi %1, %1, 4 \n" // Increment source pointer (4 bytes)
        : "+r"(dest), "+r"(src)
        :
        : "t0", "memory"
    );

  mcycle_stop();

  uint32_t cycles = mcycle_get32()/100;

  printf("lw_sw_copy unrolled: %d cycles/iteration.\n", cycles);

  return cycles;
}

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  //Switches
  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 inputs, 4 outputs

  uint32_t do_nothing_cycles = do_nothing();
  uint32_t lw_register_loop_cycles = lw_register_loop((void *)(I2C_MASTER_BASE+I2C_ISR)); //Just picking a register without too many side effects.
  uint32_t lw_sw_copy_loop_cycles = lw_sw_copy_loop(srcBuf, dstBuf);
  uint32_t lw_sw_copy_unrolled_cycles = lw_sw_copy_unrolled(srcBuf, dstBuf);
  uint32_t lw_sw_copy_loop_vram_cycles = lw_sw_copy_loop((void*)VERA_VRAM_BASE, (void*)(VERA_VRAM_BASE+BUF_NUM_WORDS*4));

  if ((do_nothing_cycles == 8) &&
      (lw_register_loop_cycles == 12) &&
      (lw_sw_copy_loop_cycles == 15) &&
      (lw_sw_copy_unrolled_cycles == 11)) {
    printf("Test Successful.\n");
  }
  else {
    printf("Test Failed.\n");
  }

  return 0;
}
