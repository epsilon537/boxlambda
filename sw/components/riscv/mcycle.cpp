
#include "mcycle.h"

#include <stdint.h>

/**
 * Delay loop executing within 8 cycles on ibex
 */
static void delay_loop_ibex(unsigned long loops) {
  int out; /* only to notify compiler of modifications to |loops| */
  asm volatile(
            "1: nop             \n" // 1 cycle
            "   nop             \n" // 1 cycle
            "   nop             \n" // 1 cycle
            "   nop             \n" // 1 cycle
            "   addi %1, %1, -1 \n" // 1 cycle
            "   bnez %1, 1b     \n" // 3 cycles
            : "=&r" (out)
            : "0" (loops)
            );
}

static int usleep_ibex(unsigned long usec) {
  unsigned long usec_cycles;
  usec_cycles = PLATFORM_CLK_FREQ * usec / 1000 / 1000 / 8;

  delay_loop_ibex(usec_cycles);
  return 0;
}

int usleep(unsigned long usec) {
  return usleep_ibex(usec);
}

void pcount_reset() {
  asm volatile(
            "csrw minstret,       x0\n"
            "csrw mcycle,         x0\n"
            "csrw mhpmcounter3,   x0\n"
            "csrw mhpmcounter4,   x0\n"
            "csrw mhpmcounter5,   x0\n"
            "csrw mhpmcounter6,   x0\n"
            "csrw mhpmcounter7,   x0\n"
            "csrw mhpmcounter8,   x0\n"
            "csrw mhpmcounter9,   x0\n"
            "csrw mhpmcounter10,  x0\n"
            "csrw mhpmcounter11,  x0\n"
            "csrw mhpmcounter12,  x0\n"
            "csrw mhpmcounter13,  x0\n"
            "csrw mhpmcounter14,  x0\n"
            "csrw mhpmcounter15,  x0\n"
            "csrw mhpmcounter16,  x0\n"
            "csrw mhpmcounter17,  x0\n"
            "csrw mhpmcounter18,  x0\n"
            "csrw mhpmcounter19,  x0\n"
            "csrw mhpmcounter20,  x0\n"
            "csrw mhpmcounter21,  x0\n"
            "csrw mhpmcounter22,  x0\n"
            "csrw mhpmcounter23,  x0\n"
            "csrw mhpmcounter24,  x0\n"
            "csrw mhpmcounter25,  x0\n"
            "csrw mhpmcounter26,  x0\n"
            "csrw mhpmcounter27,  x0\n"
            "csrw mhpmcounter28,  x0\n"
            "csrw mhpmcounter29,  x0\n"
            "csrw mhpmcounter30,  x0\n"
            "csrw mhpmcounter31,  x0\n"
            "csrw minstreth,      x0\n"
            "csrw mcycleh,        x0\n"
            "csrw mhpmcounter3h,  x0\n"
            "csrw mhpmcounter4h,  x0\n"
            "csrw mhpmcounter5h,  x0\n"
            "csrw mhpmcounter6h,  x0\n"
            "csrw mhpmcounter7h,  x0\n"
            "csrw mhpmcounter8h,  x0\n"
            "csrw mhpmcounter9h,  x0\n"
            "csrw mhpmcounter10h, x0\n"
            "csrw mhpmcounter11h, x0\n"
            "csrw mhpmcounter12h, x0\n"
            "csrw mhpmcounter13h, x0\n"
            "csrw mhpmcounter14h, x0\n"
            "csrw mhpmcounter15h, x0\n"
            "csrw mhpmcounter16h, x0\n"
            "csrw mhpmcounter17h, x0\n"
            "csrw mhpmcounter18h, x0\n"
            "csrw mhpmcounter19h, x0\n"
            "csrw mhpmcounter20h, x0\n"
            "csrw mhpmcounter21h, x0\n"
            "csrw mhpmcounter22h, x0\n"
            "csrw mhpmcounter23h, x0\n"
            "csrw mhpmcounter24h, x0\n"
            "csrw mhpmcounter25h, x0\n"
            "csrw mhpmcounter26h, x0\n"
            "csrw mhpmcounter27h, x0\n"
            "csrw mhpmcounter28h, x0\n"
            "csrw mhpmcounter29h, x0\n"
            "csrw mhpmcounter30h, x0\n"
            "csrw mhpmcounter31h, x0\n");
}

void mcycle_stop() {
  asm volatile ("csrw 0x320, %0" : : "r" (0xFFFFFFFF));
}

void mcycle_start() {
  asm volatile ("csrw 0x320, %0" : : "r" (0));
}

uint32_t mcycle_get32() {
  volatile uint32_t result;
  asm volatile ("csrr %0, 0xB00" : "=r" (result));
  return result;
}

uint64_t mcycle_get64() {
  volatile uint64_t result;
  volatile uint32_t lower;
  // lower bits
  asm volatile ("csrr %0, 0xB00" : "=r" (lower));
  // higher bits
  asm volatile ("csrr %0, 0xB80" : "=r" (result));
  result <<= 32;
  result += lower;
  return result;
}

uint64_t cc2us(uint64_t clock_cycle) {
  return clock_cycle / (PLATFORM_CLK_FREQ / 1000000);
}
