#include "j1b_hal.h"
#include <assert.h>

//progLen must be a multiple of 4 and <= J1B_PROG_SIZE_BYTES.
void j1b_load_program(unsigned char *progData, unsigned progLen) {
	unsigned progWord;

	assert((progLen&3) == 0);
	assert(progLen <= J1B_PROG_SIZE_BYTES);

	for (int ii=0; ii<progLen; ii+=4) {
		//Byte to little endian word conversion.
		progWord = 0;
		progWord |= (unsigned)progData[ii];
		progWord |= (unsigned)(progData[ii+1])<<8;
		progWord |= (unsigned)(progData[ii+2])<<16;
		progWord |= (unsigned)(progData[ii+3])<<24;

		((unsigned volatile *)(J1B_PROG_MEM_BASE))[ii/4] = progWord;
	}
}
