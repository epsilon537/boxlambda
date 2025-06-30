#ifndef VS0_HAL_H
#define VS0_HAL_H

#include <assert.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Virtual Socket 0 abstract hardware access layer.
 * This HAL contains some definitions that are common across all Virtual Socket 0
 * component variants (vs0_j1b, vs0_stub,...)
 */

//Base address of the VS0 memory space.
#define VS0_BASE 0x13000000
//Size in words of the VS0 memory space.
#define VS0_SIZE_WORDS (0x100000>>2)

//VS0 signature register offset used to identify exactl which VS0 module is
//active in the system.
#define VS0_REG_SIGNATURE (VS0_SIZE_WORDS-1)

#define VS0_REG_MAX VS0_REG_SIGNATURE

/* Technically, this belong into a separata HAL called vs0_stub because it applies */
/* to the vs0_stub component only. This is the only definition specific to the */
/* vs0_stub component, however, so I'm just going to keep it here for the time being. */
#define VS0_STUB_SIG_VALUE 0x0000510b

//Write to Register
inline void vs0_reg_wr(uint32_t reg_offset, uint32_t val)
{
	assert(reg_offset <= VS0_REG_MAX);
	((uint32_t volatile *)(VS0_BASE))[reg_offset] = val;
}

//Read from Register
inline uint32_t vs0_reg_rd(uint32_t reg_offset)
{
	assert(reg_offset <= VS0_REG_MAX);
	return ((uint32_t volatile *)(VS0_BASE))[reg_offset];
}

#ifdef __cplusplus
}
#endif

#endif //VS0_HAL_H
