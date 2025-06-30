#ifndef TIMER_H
#define TIMER_H

#include <stdint.h>
#include "mtimer_regs.h"

#define MTIMER_FREQ_HZ 50000000

#define MTIMER_SECONDS_TO_CLOCKS(SEC) \
    ((uint64_t)(((SEC) * (MTIMER_FREQ_HZ))))

#define MTIMER_MSEC_TO_CLOCKS(MSEC) \
    ((uint64_t)(((MSEC) * (MTIMER_FREQ_HZ)) / 1000))

#define MTIMER_USEC_TO_CLOCKS(USEC) \
    ((uint64_t)(((USEC) * (MTIMER_FREQ_HZ)) / 1000000))

/**
 * Minimal overhead accessor to retrieve the low word of MTIME.
 **/
#define MTIMER_GET_RAW_MTIME_LOW() (MTIMER->MTIME)

/**
 * Minimal overhead accessor to retrieve the low word of MTIMECMP.
 * Used in conjunction with MTIMER_BLK_UNTIL() to remove interrupt jitter from timer interrupts.
 **/
#define MTIMER_GET_RAW_TIMECMP_LOW() (MTIMER->MTIMECMP)

/**
 * Block until the lower 8 bits of the mtime are equal to the argument.
 * This is accurate to 1 clock cycle.
 * This mechanisf can be used to remove interrupt jitter from timer interrupts.
 **/
#define MTIMER_BLK_UNTIL(t) (MTIMER->MTIMEBLK = (t))

#ifdef __cplusplus
extern "C" {
#endif
/**
 * Disable timer compare point by setting it to max. value
 */
void mtimer_disable_raw_time_cmp(void);

/** Set the raw time compare point in system timer clocks.
 * @param clock_offset Time relative to current mtime when
 * @note The time range of the 64 bit timer is large enough not to consider a wrap around of mtime.
 * A timer interrupt will be generated at mtime + clock_offset.
 */
void mtimer_set_raw_time_cmp(uint64_t clock_offset);

/** Read the raw time of the system timer in system timer clocks
 */
uint64_t mtimer_get_raw_time(void);

#ifdef __cplusplus
}
#endif

#endif// #ifdef TIMER_H
