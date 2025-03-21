#ifndef YM2149_SYS_REGS_H
#define YM2149_SYS_REGS_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Dual YM2149 hardware access layer.
 */
#define YM2149_SYS_BASE 0x10001000

//PSG0 registers
#define PSG0_CHA_TONE_PERIOD_FINE_OFFSET   0  // 8 BITS
#define PSG0_CHA_TONE_PERIOD_COARSE_OFFSET 1  // 4 BITS
#define PSG0_CHB_TONE_PERIOD_FINE_OFFSET   2  // 8 BITS
#define PSG0_CHB_TONE_PERIOD_COARSE_OFFSET 3  // 4 BITS
#define PSG0_CHC_TONE_PERIOD_FINE_OFFSET   4  // 8 BITS
#define PSG0_CHC_TONE_PERIOD_COARSE_OFFSET 5  // 4 BITS
#define PSG0_NOISE_PERIOD_OFFSET           6  // 5 BITS
#define PSG0_ENABLE_OFFSET                 7  // 8 BITS
#define PSG0_CHA_AMPLITUDE                 8  // 5 BITS
#define PSG0_CHB_AMPLITUDE                 9  // 5 BITS
#define PSG0_CHC_AMPLITUDE                 10 // 5 BITS
#define PSG0_ENV_PERIOD_FINE_OFFSET        11 // 8 BITS
#define PSG0_ENV_PERIOD_COARSE_OFFSET      12 // 8 BITS
#define PSG0_ENV_SHAPE_CYCLE               13 // 4 BITS
#define PSG0_IO_PORT_A_OFFSET              14 // 8 BITS
#define PSG0_IO_PORT_B_OFFSET              15 // 8 BITS

//PSG1 registers
#define PSG1_CHA_TONE_PERIOD_FINE_OFFSET   (16+0)  // 8 BITS
#define PSG1_CHA_TONE_PERIOD_COARSE_OFFSET (16+1)  // 4 BITS
#define PSG1_CHB_TONE_PERIOD_FINE_OFFSET   (16+2)  // 8 BITS
#define PSG1_CHB_TONE_PERIOD_COARSE_OFFSET (16+3)  // 4 BITS
#define PSG1_CHC_TONE_PERIOD_FINE_OFFSET   (16+4)  // 8 BITS
#define PSG1_CHC_TONE_PERIOD_COARSE_OFFSET (16+5)  // 4 BITS
#define PSG1_NOISE_PERIOD_OFFSET           (16+6)  // 5 BITS
#define PSG1_ENABLE_OFFSET                 (16+7)  // 8 BITS
#define PSG1_CHA_AMPLITUDE                 (16+8)  // 5 BITS
#define PSG1_CHB_AMPLITUDE                 (16+9)  // 5 BITS
#define PSG1_CHC_AMPLITUDE                 (16+10) // 5 BITS
#define PSG1_ENV_PERIOD_FINE_OFFSET        (16+11) // 8 BITS
#define PSG1_ENV_PERIOD_COARSE_OFFSET      (16+12)// 8 BITS
#define PSG1_ENV_SHAPE_CYCLE               (16+13) // 4 BITS
#define PSG1_IO_PORT_A_OFFSET              (16+14) // 8 BITS
#define PSG1_IO_PORT_B_OFFSET              (16+15) // 8 BITS

//Audio Mixer registers
//Individual channel volumes
#define FILTER_MIXER_VOLA_OFFSET (128+0)
#define FILTER_MIXER_VOLB_OFFSET (128+1)
#define FILTER_MIXER_VOLC_OFFSET (128+2)
#define FILTER_MIXER_VOLD_OFFSET (128+3)
#define FILTER_MIXER_VOLE_OFFSET (128+4)
#define FILTER_MIXER_VOLF_OFFSET (128+5)

//Master volume
#define FILTER_MIXER_MVOL_OFFSET (128+6)
#define FILTER_MIXER_INV_OFFSET  (128+7)

//Bass
#define FILTER_MIXER_BASS_OFFSET (128+8)

//Treble
#define FILTER_MIXER_TREB_OFFSET (128+9)

inline void ym2149_sys_reg_wr(unsigned reg_offset, unsigned val)
{
	((unsigned volatile *)(YM2149_SYS_BASE))[reg_offset] = val;
}

#ifdef __cplusplus
}
#endif

#endif //YM2149_SYS_REGS_H