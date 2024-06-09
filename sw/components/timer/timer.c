/*
   Simple machine mode timer driver for RISC-V standard timer.
   SPDX-License-Identifier: Unlicense

   (https://five-embeddev.com/)

   This module is taken from five-embeddev's c-hardware-access-riscv repo:
   https://github.com/five-embeddev/c-hardware-access-riscv/tree/main
*/

#include "timer.h"

// NOLINTBEGIN (performance-no-int-to-ptr)
// performance-no-int-to-ptr: MMIO Address represented as integer is converted to pointer.

void mtimer_disable_raw_time_cmp(void) {
    volatile uint32_t* const mtimecmpl = (volatile uint32_t*)(MTIMECMP_ADDR);
    volatile uint32_t* const mtimecmph = (volatile uint32_t*)(MTIMECMP_ADDR + 4);
    *mtimecmph = 0xFFFFFFFFUL;
    *mtimecmpl = 0xFFFFFFFFUL;
}

void mtimer_set_raw_time_cmp(uint64_t clock_offset) {
    // First of all set
    uint64_t new_mtimecmp = mtimer_get_raw_time() + clock_offset;
    volatile uint32_t* const mtimecmpl = (volatile uint32_t*)(MTIMECMP_ADDR);
    volatile uint32_t* const mtimecmph = (volatile uint32_t*)(MTIMECMP_ADDR + 4);
    // AS we are doing 32 bit writes, an intermediate mtimecmp value may cause spurious interrupts.
    // Prevent that by first setting the dummy MSB to an unacheivable value
    *mtimecmph = 0xFFFFFFFFUL;// cppcheck-suppress redundantAssignment
    // set the LSB
    *mtimecmpl = (uint32_t)(new_mtimecmp & 0x0FFFFFFFFUL);
    // Set the correct MSB
    *mtimecmph = (uint32_t)(new_mtimecmp >> 32UL);// cppcheck-suppress redundantAssignment
}

/** Read the raw time of the system timer in system timer clocks
 */
uint64_t mtimer_get_raw_time(void) {
    volatile const uint32_t* const mtimel = (volatile uint32_t*)(MTIME_ADDR);
    volatile const uint32_t* const mtimeh = (volatile uint32_t*)(MTIME_ADDR + 4);
    uint32_t mtimeh_val = 0;
    uint32_t mtimel_val = 0;
    do {
        // There is a small risk the mtimeh will tick over after reading mtimel
        mtimeh_val = *mtimeh;
        mtimel_val = *mtimel;
        // Poll mtimeh to ensure it's consistent after reading mtimel
        // The frequency of mtimeh ticking over is low
    } while (mtimeh_val != *mtimeh);
    return (uint64_t)((((uint64_t)mtimeh_val) << 32UL) | mtimel_val);
}

// NOLINTEND (performance-no-int-to-ptr)
