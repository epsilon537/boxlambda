#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "i2c_master_regs.h"
#include "vera_hal.h"
#include "interrupts.h"
#include "timer.h"

//The disassembly of the first few instructions in the timer interrupt handler looks like this:
//00000c48 <_timer_irq_handler>:
//     c48:       100207b7                lui     a5,0x10020
//     c4c:       0007a703                lw      a4,0(a5) # 10020000
//
//The lui and lw instructions each take 2 cycles to execute.
//In other words, _timer_irq_handle started 4 clock cycles before
//the retrieved mtimer value.
#define IRQ_LATENCY_OFFSET 4

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

#define BUF_NUM_WORDS 4096

#define NUM_ITERATIONS 100

static uint32_t srcBuf[BUF_NUM_WORDS];
static uint32_t dstBuf[BUF_NUM_WORDS];

//Measure how many cycles it take to call mcycle_start() and mcycle_stop().
uint32_t do_nothing() {
  mcycle_stop();
  pcount_reset();
  mcycle_start();
  mcycle_stop();

  uint32_t cycles = mcycle_get32();

  printf("Do nothing: %d cycles.\n", cycles);

  return cycles;
}

//Repeatedly read a SoC register and measure how long it takes.
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

//Copy NUM_ITERATIONS words from src to dest using a naive loop and measure how long it takes.
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

//NUM_ITERATIONS of the lw_sw_copy_loop unrolled
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
  disable_all_irqs();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

volatile uint32_t timel = 0;

void _timer_irq_handler(void) {
  timel = MTIMER->MTIME; //Retrieve current time, for check at thread-level after returning from ISR.

  mtimer_disable_raw_time_cmp(); //Disable raw_time_cmp to prevent IRQ from re-firing.

  //Return from interrupt - ISRs in BoxLambda have the "naked" attribute which means we have to provide the proper prologue and epilogue ourselves.
  __asm__ volatile (
      "mret \n"
  );
}

uint32_t irq_latency_max=0;
uint32_t irq_latency_min=~0;

void irq_latency_test(void) {
  printf("Enabling Timer IRQ.\n");
  enable_global_irq();
  enable_irq(IRQ_ID_TIMER);

  int ii;
  uint32_t time_cmpl;
  uint32_t irq_latency;

  for (ii=0; ii<50; ii++) {
    timel = 0;
    mtimer_set_raw_time_cmp(1000+ii); //Fire IRQ in 1000+ii ticks.
    time_cmpl = MTIMER->MTIMECMP; //The comparator value.

    while (timel == 0); //Wait for the IRQ.

    irq_latency = timel-time_cmpl-IRQ_LATENCY_OFFSET;

    //There is a window due to IRQ jitter.
    if (irq_latency < irq_latency_min) irq_latency_min = irq_latency;
    if (irq_latency > irq_latency_max) irq_latency_max = irq_latency;
  }

  printf("Timer IRQ latency Min-Max: %d-%d cycles.\n", irq_latency_min, irq_latency_max);
  printf("Expected: 5-7 cycles.\n");

  disable_irq(IRQ_ID_TIMER);
  disable_global_irq();
}

int main(void) {
  //Switches
  gpio_init();
  gpio_set_direction(0x0000000F); //4 inputs, 4 outputs

  irq_latency_test();

  uint32_t do_nothing_cycles = do_nothing();
  printf("Expected: 8 cycles.\n");
  uint32_t lw_register_loop_cycles = lw_register_loop((void *)&(I2C_MASTER->ISR)); //Just picking a register without too many side effects.
  printf("Expected: 8 cycles.\n");
  uint32_t lw_sw_copy_loop_cycles = lw_sw_copy_loop(srcBuf, dstBuf);
  printf("Expected: 14 cycles.\n");
  uint32_t lw_sw_copy_unrolled_cycles = lw_sw_copy_unrolled(srcBuf, dstBuf);
  printf("Expected: 8 cycles.\n");
  uint32_t lw_sw_copy_loop_vram_cycles = lw_sw_copy_loop((void*)VERA_VRAM_BASE, (void*)(VERA_VRAM_BASE+BUF_NUM_WORDS*4));
  printf("Expected: 14.\n");

  if ((irq_latency_min == 5) &&
      (irq_latency_max == 7) &&
      (do_nothing_cycles == 8) &&
      (lw_register_loop_cycles == 8) &&
      (lw_sw_copy_loop_cycles == 14) &&
      (lw_sw_copy_unrolled_cycles == 8) &&
      (lw_sw_copy_loop_vram_cycles == 14)) {
    printf("Test Successful.\n");
  }
  else {
    printf("Test Failed.\n");
  }

  return 0;
}
