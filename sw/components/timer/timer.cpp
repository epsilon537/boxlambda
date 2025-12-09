#include "timer.h"

void mtimer_disable_raw_time_cmp(void) {
    MTIMER->MTIMECMPH = 0xFFFFFFFFUL;
    MTIMER->MTIMECMP = 0xFFFFFFFFUL;
}

void mtimer_set_raw_time_cmp(uint64_t clock_offset) {
    // First of all set
    uint64_t new_mtimecmp = mtimer_get_raw_time() + clock_offset;
    // AS we are doing 32 bit writes, an intermediate mtimecmp value may cause spurious interrupts.
    // Prevent that by first setting the dummy MSB to an unacheivable value
    MTIMER->MTIMECMPH = 0xFFFFFFFFUL;// cppcheck-suppress redundantAssignment
    // set the LSB
    MTIMER->MTIMECMP = (uint32_t)(new_mtimecmp & 0x0FFFFFFFFUL);
    // Set the correct MSB
    MTIMER->MTIMECMPH = (uint32_t)(new_mtimecmp >> 32UL);
}

/** Read the raw time of the system timer in system timer clocks
 */
uint64_t mtimer_get_raw_time(void) {
    uint32_t mtimeh_val = 0;
    uint32_t mtimel_val = 0;
    do {
        // There is a small risk the mtimeh will tick over after reading mtimel
        mtimeh_val = MTIMER->MTIMEH;
        mtimel_val = MTIMER->MTIME;
        // Poll mtimeh to ensure it's consistent after reading mtimel
        // The frequency of mtimeh ticking over is low
    } while (mtimeh_val != MTIMER->MTIMEH);
    return (uint64_t)((((uint64_t)mtimeh_val) << 32UL) | mtimel_val);
}

