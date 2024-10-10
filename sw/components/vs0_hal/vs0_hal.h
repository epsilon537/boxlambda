#ifndef VS0_HAL_H
#define VS0_HAL_H

#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * VS0 hardware access layer.
 */
#define VS0_BASE 0x13000000
#define VS0_SIZE_WORDS (0x100000>>2)

#define VS0_REG_SIGNATURE (VS0_SIZE_WORDS-1)

#define VS0_REG_MAX VS0_REG_SIGNATURE

#define VS0_STUB_SIG_VALUE 0x0000510b

//Write to Register
inline void vs0_reg_wr(unsigned reg_offset, unsigned val)
{
	assert(reg_offset <= VS0_REG_MAX);
	((unsigned volatile *)(VS0_BASE))[reg_offset] = val;
}

//Read from Register
inline unsigned vs0_reg_rd(unsigned reg_offset)
{
	assert(reg_offset <= VS0_REG_MAX);
	return ((unsigned volatile *)(VS0_BASE))[reg_offset];
}

#ifdef __cplusplus
}
#endif

#endif //VS0_HAL_H
