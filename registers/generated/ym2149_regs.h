// Created with Corsair v1.0.4
#ifndef __YM2149_REGS_H
#define __YM2149_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define YM2149_BASE_ADDR 0x10001000

// PSG0_CHA_TONE_PERIOD_FINE - Channel A fine tone period adjustment.
#define YM2149_PSG0_CHA_TONE_PERIOD_FINE_ADDR 0x0
#define YM2149_PSG0_CHA_TONE_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Channel A fine tone period adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg0_cha_tone_period_fine_t;

// PSG0_CHA_TONE_PERIOD_FINE.VALUE - Channel A fine tone period adjustment.
#define YM2149_PSG0_CHA_TONE_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG0_CHA_TONE_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG0_CHA_TONE_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG0_CHA_TONE_PERIOD_FINE_VALUE_RESET 0x0

// PSG0_CHA_TONE_PERIOD_COARSE - Channel A coarse tone period adjustment.
#define YM2149_PSG0_CHA_TONE_PERIOD_COARSE_ADDR 0x4
#define YM2149_PSG0_CHA_TONE_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Channel A coarse tone period adjustment.
    uint32_t : 28; // reserved
  };
} ym2149_psg0_cha_tone_period_coarse_t;

// PSG0_CHA_TONE_PERIOD_COARSE.VALUE - Channel A coarse tone period adjustment.
#define YM2149_PSG0_CHA_TONE_PERIOD_COARSE_VALUE_WIDTH 4
#define YM2149_PSG0_CHA_TONE_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG0_CHA_TONE_PERIOD_COARSE_VALUE_MASK 0xf
#define YM2149_PSG0_CHA_TONE_PERIOD_COARSE_VALUE_RESET 0x0

// PSG0_CHB_TONE_PERIOD_FINE - Channel B fine tone period adjustment.
#define YM2149_PSG0_CHB_TONE_PERIOD_FINE_ADDR 0x8
#define YM2149_PSG0_CHB_TONE_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Channel B fine tone period adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg0_chb_tone_period_fine_t;

// PSG0_CHB_TONE_PERIOD_FINE.VALUE - Channel B fine tone period adjustment.
#define YM2149_PSG0_CHB_TONE_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG0_CHB_TONE_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG0_CHB_TONE_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG0_CHB_TONE_PERIOD_FINE_VALUE_RESET 0x0

// PSG0_CHB_TONE_PERIOD_COARSE - Channel B coarse tone period adjustment.
#define YM2149_PSG0_CHB_TONE_PERIOD_COARSE_ADDR 0xc
#define YM2149_PSG0_CHB_TONE_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Channel B coarse tone period adjustment.
    uint32_t : 28; // reserved
  };
} ym2149_psg0_chb_tone_period_coarse_t;

// PSG0_CHB_TONE_PERIOD_COARSE.VALUE - Channel B coarse tone period adjustment.
#define YM2149_PSG0_CHB_TONE_PERIOD_COARSE_VALUE_WIDTH 4
#define YM2149_PSG0_CHB_TONE_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG0_CHB_TONE_PERIOD_COARSE_VALUE_MASK 0xf
#define YM2149_PSG0_CHB_TONE_PERIOD_COARSE_VALUE_RESET 0x0

// PSG0_CHC_TONE_PERIOD_FINE - Channel C fine tone period adjustment.
#define YM2149_PSG0_CHC_TONE_PERIOD_FINE_ADDR 0x10
#define YM2149_PSG0_CHC_TONE_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Channel C fine tone period adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg0_chc_tone_period_fine_t;

// PSG0_CHC_TONE_PERIOD_FINE.VALUE - Channel C fine tone period adjustment.
#define YM2149_PSG0_CHC_TONE_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG0_CHC_TONE_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG0_CHC_TONE_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG0_CHC_TONE_PERIOD_FINE_VALUE_RESET 0x0

// PSG0_CHC_TONE_PERIOD_COARSE - Channel C coarse tone period adjustment.
#define YM2149_PSG0_CHC_TONE_PERIOD_COARSE_ADDR 0x14
#define YM2149_PSG0_CHC_TONE_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Channel C coarse tone period adjustment.
    uint32_t : 28; // reserved
  };
} ym2149_psg0_chc_tone_period_coarse_t;

// PSG0_CHC_TONE_PERIOD_COARSE.VALUE - Channel C coarse tone period adjustment.
#define YM2149_PSG0_CHC_TONE_PERIOD_COARSE_VALUE_WIDTH 4
#define YM2149_PSG0_CHC_TONE_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG0_CHC_TONE_PERIOD_COARSE_VALUE_MASK 0xf
#define YM2149_PSG0_CHC_TONE_PERIOD_COARSE_VALUE_RESET 0x0

// PSG0_NOISE_PERIOD - Noise period.
#define YM2149_PSG0_NOISE_PERIOD_ADDR 0x18
#define YM2149_PSG0_NOISE_PERIOD_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 5; // Noise period.
    uint32_t : 27; // reserved
  };
} ym2149_psg0_noise_period_t;

// PSG0_NOISE_PERIOD.VALUE - Noise period.
#define YM2149_PSG0_NOISE_PERIOD_VALUE_WIDTH 5
#define YM2149_PSG0_NOISE_PERIOD_VALUE_LSB 0
#define YM2149_PSG0_NOISE_PERIOD_VALUE_MASK 0x1f
#define YM2149_PSG0_NOISE_PERIOD_VALUE_RESET 0x0

// PSG0_DISABLE - Mixer settings.
#define YM2149_PSG0_DISABLE_ADDR 0x1c
#define YM2149_PSG0_DISABLE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t TONE_A : 1; // Channel A tone disable.
    uint32_t TONE_B : 1; // Channel B tone disable.
    uint32_t TONE_C : 1; // Channel C tone disable.
    uint32_t NOISE_A : 1; // Channel A noise disable.
    uint32_t NOISE_B : 1; // Channel B noise disable.
    uint32_t NOISE_C : 1; // Channel C noise disable.
    uint32_t : 26; // reserved
  };
} ym2149_psg0_disable_t;

// PSG0_DISABLE.TONE_A - Channel A tone disable.
#define YM2149_PSG0_DISABLE_TONE_A_WIDTH 1
#define YM2149_PSG0_DISABLE_TONE_A_LSB 0
#define YM2149_PSG0_DISABLE_TONE_A_MASK 0x1
#define YM2149_PSG0_DISABLE_TONE_A_RESET 0x0

// PSG0_DISABLE.TONE_B - Channel B tone disable.
#define YM2149_PSG0_DISABLE_TONE_B_WIDTH 1
#define YM2149_PSG0_DISABLE_TONE_B_LSB 1
#define YM2149_PSG0_DISABLE_TONE_B_MASK 0x2
#define YM2149_PSG0_DISABLE_TONE_B_RESET 0x0

// PSG0_DISABLE.TONE_C - Channel C tone disable.
#define YM2149_PSG0_DISABLE_TONE_C_WIDTH 1
#define YM2149_PSG0_DISABLE_TONE_C_LSB 2
#define YM2149_PSG0_DISABLE_TONE_C_MASK 0x4
#define YM2149_PSG0_DISABLE_TONE_C_RESET 0x0

// PSG0_DISABLE.NOISE_A - Channel A noise disable.
#define YM2149_PSG0_DISABLE_NOISE_A_WIDTH 1
#define YM2149_PSG0_DISABLE_NOISE_A_LSB 3
#define YM2149_PSG0_DISABLE_NOISE_A_MASK 0x8
#define YM2149_PSG0_DISABLE_NOISE_A_RESET 0x0

// PSG0_DISABLE.NOISE_B - Channel B noise disable.
#define YM2149_PSG0_DISABLE_NOISE_B_WIDTH 1
#define YM2149_PSG0_DISABLE_NOISE_B_LSB 4
#define YM2149_PSG0_DISABLE_NOISE_B_MASK 0x10
#define YM2149_PSG0_DISABLE_NOISE_B_RESET 0x0

// PSG0_DISABLE.NOISE_C - Channel C noise disable.
#define YM2149_PSG0_DISABLE_NOISE_C_WIDTH 1
#define YM2149_PSG0_DISABLE_NOISE_C_LSB 5
#define YM2149_PSG0_DISABLE_NOISE_C_MASK 0x20
#define YM2149_PSG0_DISABLE_NOISE_C_RESET 0x0

// PSG0_CHA_LVL - Channel A level control.
#define YM2149_PSG0_CHA_LVL_ADDR 0x20
#define YM2149_PSG0_CHA_LVL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LVL : 4; // Level selection.
    uint32_t MODE : 1; // Mode.
    uint32_t : 27; // reserved
  };
} ym2149_psg0_cha_lvl_t;

// PSG0_CHA_LVL.LVL - Level selection.
#define YM2149_PSG0_CHA_LVL_LVL_WIDTH 4
#define YM2149_PSG0_CHA_LVL_LVL_LSB 0
#define YM2149_PSG0_CHA_LVL_LVL_MASK 0xf
#define YM2149_PSG0_CHA_LVL_LVL_RESET 0x0

// PSG0_CHA_LVL.MODE - Mode.
#define YM2149_PSG0_CHA_LVL_MODE_WIDTH 1
#define YM2149_PSG0_CHA_LVL_MODE_LSB 4
#define YM2149_PSG0_CHA_LVL_MODE_MASK 0x10
#define YM2149_PSG0_CHA_LVL_MODE_RESET 0x0

// PSG0_CHB_LVL - Channel B level control.
#define YM2149_PSG0_CHB_LVL_ADDR 0x24
#define YM2149_PSG0_CHB_LVL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LVL : 4; // Level selection.
    uint32_t MODE : 1; // Mode.
    uint32_t : 27; // reserved
  };
} ym2149_psg0_chb_lvl_t;

// PSG0_CHB_LVL.LVL - Level selection.
#define YM2149_PSG0_CHB_LVL_LVL_WIDTH 4
#define YM2149_PSG0_CHB_LVL_LVL_LSB 0
#define YM2149_PSG0_CHB_LVL_LVL_MASK 0xf
#define YM2149_PSG0_CHB_LVL_LVL_RESET 0x0

// PSG0_CHB_LVL.MODE - Mode.
#define YM2149_PSG0_CHB_LVL_MODE_WIDTH 1
#define YM2149_PSG0_CHB_LVL_MODE_LSB 4
#define YM2149_PSG0_CHB_LVL_MODE_MASK 0x10
#define YM2149_PSG0_CHB_LVL_MODE_RESET 0x0

// PSG0_CHC_LVL - Channel C level control.
#define YM2149_PSG0_CHC_LVL_ADDR 0x28
#define YM2149_PSG0_CHC_LVL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LVL : 4; // Level selection.
    uint32_t MODE : 1; // Mode.
    uint32_t : 27; // reserved
  };
} ym2149_psg0_chc_lvl_t;

// PSG0_CHC_LVL.LVL - Level selection.
#define YM2149_PSG0_CHC_LVL_LVL_WIDTH 4
#define YM2149_PSG0_CHC_LVL_LVL_LSB 0
#define YM2149_PSG0_CHC_LVL_LVL_MASK 0xf
#define YM2149_PSG0_CHC_LVL_LVL_RESET 0x0

// PSG0_CHC_LVL.MODE - Mode.
#define YM2149_PSG0_CHC_LVL_MODE_WIDTH 1
#define YM2149_PSG0_CHC_LVL_MODE_LSB 4
#define YM2149_PSG0_CHC_LVL_MODE_MASK 0x10
#define YM2149_PSG0_CHC_LVL_MODE_RESET 0x0

// PSG0_ENV_PERIOD_FINE - Envelope frequency fine adjustment.
#define YM2149_PSG0_ENV_PERIOD_FINE_ADDR 0x2c
#define YM2149_PSG0_ENV_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Envelope frequency fine adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg0_env_period_fine_t;

// PSG0_ENV_PERIOD_FINE.VALUE - Envelope frequency fine adjustment.
#define YM2149_PSG0_ENV_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG0_ENV_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG0_ENV_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG0_ENV_PERIOD_FINE_VALUE_RESET 0x0

// PSG0_ENV_PERIOD_COARSE - Envelope frequeny coarse adjustment.
#define YM2149_PSG0_ENV_PERIOD_COARSE_ADDR 0x30
#define YM2149_PSG0_ENV_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Envelope frequency coarse adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg0_env_period_coarse_t;

// PSG0_ENV_PERIOD_COARSE.VALUE - Envelope frequency coarse adjustment.
#define YM2149_PSG0_ENV_PERIOD_COARSE_VALUE_WIDTH 8
#define YM2149_PSG0_ENV_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG0_ENV_PERIOD_COARSE_VALUE_MASK 0xff
#define YM2149_PSG0_ENV_PERIOD_COARSE_VALUE_RESET 0x0

// PSG0_ENV_SHAPE - Envelope shape.
#define YM2149_PSG0_ENV_SHAPE_ADDR 0x34
#define YM2149_PSG0_ENV_SHAPE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Envelope shape.
    uint32_t : 28; // reserved
  };
} ym2149_psg0_env_shape_t;

// PSG0_ENV_SHAPE.VALUE - Envelope shape.
#define YM2149_PSG0_ENV_SHAPE_VALUE_WIDTH 4
#define YM2149_PSG0_ENV_SHAPE_VALUE_LSB 0
#define YM2149_PSG0_ENV_SHAPE_VALUE_MASK 0xf
#define YM2149_PSG0_ENV_SHAPE_VALUE_RESET 0x0

// PSG1_CHA_TONE_PERIOD_FINE - Channel A fine tone period adjustment.
#define YM2149_PSG1_CHA_TONE_PERIOD_FINE_ADDR 0x40
#define YM2149_PSG1_CHA_TONE_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Channel A fine tone period adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg1_cha_tone_period_fine_t;

// PSG1_CHA_TONE_PERIOD_FINE.VALUE - Channel A fine tone period adjustment.
#define YM2149_PSG1_CHA_TONE_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG1_CHA_TONE_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG1_CHA_TONE_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG1_CHA_TONE_PERIOD_FINE_VALUE_RESET 0x0

// PSG1_CHA_TONE_PERIOD_COARSE - Channel A coarse tone period adjustment.
#define YM2149_PSG1_CHA_TONE_PERIOD_COARSE_ADDR 0x44
#define YM2149_PSG1_CHA_TONE_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Channel A coarse tone period adjustment.
    uint32_t : 28; // reserved
  };
} ym2149_psg1_cha_tone_period_coarse_t;

// PSG1_CHA_TONE_PERIOD_COARSE.VALUE - Channel A coarse tone period adjustment.
#define YM2149_PSG1_CHA_TONE_PERIOD_COARSE_VALUE_WIDTH 4
#define YM2149_PSG1_CHA_TONE_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG1_CHA_TONE_PERIOD_COARSE_VALUE_MASK 0xf
#define YM2149_PSG1_CHA_TONE_PERIOD_COARSE_VALUE_RESET 0x0

// PSG1_CHB_TONE_PERIOD_FINE - Channel B fine tone period adjustment.
#define YM2149_PSG1_CHB_TONE_PERIOD_FINE_ADDR 0x48
#define YM2149_PSG1_CHB_TONE_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Channel B fine tone period adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg1_chb_tone_period_fine_t;

// PSG1_CHB_TONE_PERIOD_FINE.VALUE - Channel B fine tone period adjustment.
#define YM2149_PSG1_CHB_TONE_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG1_CHB_TONE_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG1_CHB_TONE_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG1_CHB_TONE_PERIOD_FINE_VALUE_RESET 0x0

// PSG1_CHB_TONE_PERIOD_COARSE - Channel B coarse tone period adjustment.
#define YM2149_PSG1_CHB_TONE_PERIOD_COARSE_ADDR 0x4c
#define YM2149_PSG1_CHB_TONE_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Channel B coarse tone period adjustment.
    uint32_t : 28; // reserved
  };
} ym2149_psg1_chb_tone_period_coarse_t;

// PSG1_CHB_TONE_PERIOD_COARSE.VALUE - Channel B coarse tone period adjustment.
#define YM2149_PSG1_CHB_TONE_PERIOD_COARSE_VALUE_WIDTH 4
#define YM2149_PSG1_CHB_TONE_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG1_CHB_TONE_PERIOD_COARSE_VALUE_MASK 0xf
#define YM2149_PSG1_CHB_TONE_PERIOD_COARSE_VALUE_RESET 0x0

// PSG1_CHC_TONE_PERIOD_FINE - Channel C fine tone period adjustment.
#define YM2149_PSG1_CHC_TONE_PERIOD_FINE_ADDR 0x50
#define YM2149_PSG1_CHC_TONE_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Channel C fine tone period adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg1_chc_tone_period_fine_t;

// PSG1_CHC_TONE_PERIOD_FINE.VALUE - Channel C fine tone period adjustment.
#define YM2149_PSG1_CHC_TONE_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG1_CHC_TONE_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG1_CHC_TONE_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG1_CHC_TONE_PERIOD_FINE_VALUE_RESET 0x0

// PSG1_CHC_TONE_PERIOD_COARSE - Channel C coarse tone period adjustment.
#define YM2149_PSG1_CHC_TONE_PERIOD_COARSE_ADDR 0x54
#define YM2149_PSG1_CHC_TONE_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Channel C coarse tone period adjustment.
    uint32_t : 28; // reserved
  };
} ym2149_psg1_chc_tone_period_coarse_t;

// PSG1_CHC_TONE_PERIOD_COARSE.VALUE - Channel C coarse tone period adjustment.
#define YM2149_PSG1_CHC_TONE_PERIOD_COARSE_VALUE_WIDTH 4
#define YM2149_PSG1_CHC_TONE_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG1_CHC_TONE_PERIOD_COARSE_VALUE_MASK 0xf
#define YM2149_PSG1_CHC_TONE_PERIOD_COARSE_VALUE_RESET 0x0

// PSG1_NOISE_PERIOD - Noise period.
#define YM2149_PSG1_NOISE_PERIOD_ADDR 0x58
#define YM2149_PSG1_NOISE_PERIOD_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 5; // Noise period.
    uint32_t : 27; // reserved
  };
} ym2149_psg1_noise_period_t;

// PSG1_NOISE_PERIOD.VALUE - Noise period.
#define YM2149_PSG1_NOISE_PERIOD_VALUE_WIDTH 5
#define YM2149_PSG1_NOISE_PERIOD_VALUE_LSB 0
#define YM2149_PSG1_NOISE_PERIOD_VALUE_MASK 0x1f
#define YM2149_PSG1_NOISE_PERIOD_VALUE_RESET 0x0

// PSG1_DISABLE - Mixer settings.
#define YM2149_PSG1_DISABLE_ADDR 0x5c
#define YM2149_PSG1_DISABLE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t TONE_A : 1; // Channel A tone disable.
    uint32_t TONE_B : 1; // Channel B tone disable.
    uint32_t TONE_C : 1; // Channel C tone disable.
    uint32_t NOISE_A : 1; // Channel A noise disable.
    uint32_t NOISE_B : 1; // Channel B noise disable.
    uint32_t NOISE_C : 1; // Channel C noise disable.
    uint32_t : 26; // reserved
  };
} ym2149_psg1_disable_t;

// PSG1_DISABLE.TONE_A - Channel A tone disable.
#define YM2149_PSG1_DISABLE_TONE_A_WIDTH 1
#define YM2149_PSG1_DISABLE_TONE_A_LSB 0
#define YM2149_PSG1_DISABLE_TONE_A_MASK 0x1
#define YM2149_PSG1_DISABLE_TONE_A_RESET 0x0

// PSG1_DISABLE.TONE_B - Channel B tone disable.
#define YM2149_PSG1_DISABLE_TONE_B_WIDTH 1
#define YM2149_PSG1_DISABLE_TONE_B_LSB 1
#define YM2149_PSG1_DISABLE_TONE_B_MASK 0x2
#define YM2149_PSG1_DISABLE_TONE_B_RESET 0x0

// PSG1_DISABLE.TONE_C - Channel C tone disable.
#define YM2149_PSG1_DISABLE_TONE_C_WIDTH 1
#define YM2149_PSG1_DISABLE_TONE_C_LSB 2
#define YM2149_PSG1_DISABLE_TONE_C_MASK 0x4
#define YM2149_PSG1_DISABLE_TONE_C_RESET 0x0

// PSG1_DISABLE.NOISE_A - Channel A noise disable.
#define YM2149_PSG1_DISABLE_NOISE_A_WIDTH 1
#define YM2149_PSG1_DISABLE_NOISE_A_LSB 3
#define YM2149_PSG1_DISABLE_NOISE_A_MASK 0x8
#define YM2149_PSG1_DISABLE_NOISE_A_RESET 0x0

// PSG1_DISABLE.NOISE_B - Channel B noise disable.
#define YM2149_PSG1_DISABLE_NOISE_B_WIDTH 1
#define YM2149_PSG1_DISABLE_NOISE_B_LSB 4
#define YM2149_PSG1_DISABLE_NOISE_B_MASK 0x10
#define YM2149_PSG1_DISABLE_NOISE_B_RESET 0x0

// PSG1_DISABLE.NOISE_C - Channel C noise disable.
#define YM2149_PSG1_DISABLE_NOISE_C_WIDTH 1
#define YM2149_PSG1_DISABLE_NOISE_C_LSB 5
#define YM2149_PSG1_DISABLE_NOISE_C_MASK 0x20
#define YM2149_PSG1_DISABLE_NOISE_C_RESET 0x0

// PSG1_CHA_LVL - Channel A level control.
#define YM2149_PSG1_CHA_LVL_ADDR 0x60
#define YM2149_PSG1_CHA_LVL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LVL : 4; // Level selection.
    uint32_t MODE : 1; // Mode.
    uint32_t : 27; // reserved
  };
} ym2149_psg1_cha_lvl_t;

// PSG1_CHA_LVL.LVL - Level selection.
#define YM2149_PSG1_CHA_LVL_LVL_WIDTH 4
#define YM2149_PSG1_CHA_LVL_LVL_LSB 0
#define YM2149_PSG1_CHA_LVL_LVL_MASK 0xf
#define YM2149_PSG1_CHA_LVL_LVL_RESET 0x0

// PSG1_CHA_LVL.MODE - Mode.
#define YM2149_PSG1_CHA_LVL_MODE_WIDTH 1
#define YM2149_PSG1_CHA_LVL_MODE_LSB 4
#define YM2149_PSG1_CHA_LVL_MODE_MASK 0x10
#define YM2149_PSG1_CHA_LVL_MODE_RESET 0x0

// PSG1_CHB_LVL - Channel B level control.
#define YM2149_PSG1_CHB_LVL_ADDR 0x64
#define YM2149_PSG1_CHB_LVL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LVL : 4; // Level selection.
    uint32_t MODE : 1; // Mode.
    uint32_t : 27; // reserved
  };
} ym2149_psg1_chb_lvl_t;

// PSG1_CHB_LVL.LVL - Level selection.
#define YM2149_PSG1_CHB_LVL_LVL_WIDTH 4
#define YM2149_PSG1_CHB_LVL_LVL_LSB 0
#define YM2149_PSG1_CHB_LVL_LVL_MASK 0xf
#define YM2149_PSG1_CHB_LVL_LVL_RESET 0x0

// PSG1_CHB_LVL.MODE - Mode.
#define YM2149_PSG1_CHB_LVL_MODE_WIDTH 1
#define YM2149_PSG1_CHB_LVL_MODE_LSB 4
#define YM2149_PSG1_CHB_LVL_MODE_MASK 0x10
#define YM2149_PSG1_CHB_LVL_MODE_RESET 0x0

// PSG1_CHC_LVL - Channel C level control.
#define YM2149_PSG1_CHC_LVL_ADDR 0x68
#define YM2149_PSG1_CHC_LVL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LVL : 4; // Level selection.
    uint32_t MODE : 1; // Mode.
    uint32_t : 27; // reserved
  };
} ym2149_psg1_chc_lvl_t;

// PSG1_CHC_LVL.LVL - Level selection.
#define YM2149_PSG1_CHC_LVL_LVL_WIDTH 4
#define YM2149_PSG1_CHC_LVL_LVL_LSB 0
#define YM2149_PSG1_CHC_LVL_LVL_MASK 0xf
#define YM2149_PSG1_CHC_LVL_LVL_RESET 0x0

// PSG1_CHC_LVL.MODE - Mode.
#define YM2149_PSG1_CHC_LVL_MODE_WIDTH 1
#define YM2149_PSG1_CHC_LVL_MODE_LSB 4
#define YM2149_PSG1_CHC_LVL_MODE_MASK 0x10
#define YM2149_PSG1_CHC_LVL_MODE_RESET 0x0

// PSG1_ENV_PERIOD_FINE - Envelope frequency fine adjustment.
#define YM2149_PSG1_ENV_PERIOD_FINE_ADDR 0x6c
#define YM2149_PSG1_ENV_PERIOD_FINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Envelope frequency fine adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg1_env_period_fine_t;

// PSG1_ENV_PERIOD_FINE.VALUE - Envelope frequency fine adjustment.
#define YM2149_PSG1_ENV_PERIOD_FINE_VALUE_WIDTH 8
#define YM2149_PSG1_ENV_PERIOD_FINE_VALUE_LSB 0
#define YM2149_PSG1_ENV_PERIOD_FINE_VALUE_MASK 0xff
#define YM2149_PSG1_ENV_PERIOD_FINE_VALUE_RESET 0x0

// PSG1_ENV_PERIOD_COARSE - Envelope frequeny coarse adjustment.
#define YM2149_PSG1_ENV_PERIOD_COARSE_ADDR 0x70
#define YM2149_PSG1_ENV_PERIOD_COARSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Envelope frequency coarse adjustment.
    uint32_t : 24; // reserved
  };
} ym2149_psg1_env_period_coarse_t;

// PSG1_ENV_PERIOD_COARSE.VALUE - Envelope frequency coarse adjustment.
#define YM2149_PSG1_ENV_PERIOD_COARSE_VALUE_WIDTH 8
#define YM2149_PSG1_ENV_PERIOD_COARSE_VALUE_LSB 0
#define YM2149_PSG1_ENV_PERIOD_COARSE_VALUE_MASK 0xff
#define YM2149_PSG1_ENV_PERIOD_COARSE_VALUE_RESET 0x0

// PSG1_ENV_SHAPE - Envelope shape.
#define YM2149_PSG1_ENV_SHAPE_ADDR 0x74
#define YM2149_PSG1_ENV_SHAPE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 4; // Envelope shape.
    uint32_t : 28; // reserved
  };
} ym2149_psg1_env_shape_t;

// PSG1_ENV_SHAPE.VALUE - Envelope shape.
#define YM2149_PSG1_ENV_SHAPE_VALUE_WIDTH 4
#define YM2149_PSG1_ENV_SHAPE_VALUE_LSB 0
#define YM2149_PSG1_ENV_SHAPE_VALUE_MASK 0xf
#define YM2149_PSG1_ENV_SHAPE_VALUE_RESET 0x0

// FILTER_MIXER_VOLA - Filter mixer PSG0 channel A volume.
#define YM2149_FILTER_MIXER_VOLA_ADDR 0x200
#define YM2149_FILTER_MIXER_VOLA_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer PSG0 channel A volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_vola_t;

// FILTER_MIXER_VOLA.VALUE - Filter mixer PSG0 channel A volume.
#define YM2149_FILTER_MIXER_VOLA_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_VOLA_VALUE_LSB 0
#define YM2149_FILTER_MIXER_VOLA_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_VOLA_VALUE_RESET 0x0

// FILTER_MIXER_VOLB - Filter mixer PSG0 channel B volume.
#define YM2149_FILTER_MIXER_VOLB_ADDR 0x204
#define YM2149_FILTER_MIXER_VOLB_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer PSG0 channel B volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_volb_t;

// FILTER_MIXER_VOLB.VALUE - Filter mixer PSG0 channel B volume.
#define YM2149_FILTER_MIXER_VOLB_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_VOLB_VALUE_LSB 0
#define YM2149_FILTER_MIXER_VOLB_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_VOLB_VALUE_RESET 0x0

// FILTER_MIXER_VOLC - Filter mixer PSG0 channel C volume.
#define YM2149_FILTER_MIXER_VOLC_ADDR 0x208
#define YM2149_FILTER_MIXER_VOLC_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer PSG0 channel C volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_volc_t;

// FILTER_MIXER_VOLC.VALUE - Filter mixer PSG0 channel C volume.
#define YM2149_FILTER_MIXER_VOLC_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_VOLC_VALUE_LSB 0
#define YM2149_FILTER_MIXER_VOLC_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_VOLC_VALUE_RESET 0x0

// FILTER_MIXER_VOLD - Filter mixer PSG1 channel A volume.
#define YM2149_FILTER_MIXER_VOLD_ADDR 0x20c
#define YM2149_FILTER_MIXER_VOLD_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer PSG1 channel A volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_vold_t;

// FILTER_MIXER_VOLD.VALUE - Filter mixer PSG1 channel A volume.
#define YM2149_FILTER_MIXER_VOLD_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_VOLD_VALUE_LSB 0
#define YM2149_FILTER_MIXER_VOLD_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_VOLD_VALUE_RESET 0x0

// FILTER_MIXER_VOLE - Filter mixer PSG1 channel B volume.
#define YM2149_FILTER_MIXER_VOLE_ADDR 0x210
#define YM2149_FILTER_MIXER_VOLE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer PSG1 channel B volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_vole_t;

// FILTER_MIXER_VOLE.VALUE - Filter mixer PSG1 channel B volume.
#define YM2149_FILTER_MIXER_VOLE_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_VOLE_VALUE_LSB 0
#define YM2149_FILTER_MIXER_VOLE_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_VOLE_VALUE_RESET 0x0

// FILTER_MIXER_VOLF - Filter mixer PSG1 channel C volume.
#define YM2149_FILTER_MIXER_VOLF_ADDR 0x214
#define YM2149_FILTER_MIXER_VOLF_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer PSG1 channel C volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_volf_t;

// FILTER_MIXER_VOLF.VALUE - Filter mixer PSG1 channel C volume.
#define YM2149_FILTER_MIXER_VOLF_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_VOLF_VALUE_LSB 0
#define YM2149_FILTER_MIXER_VOLF_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_VOLF_VALUE_RESET 0x0

// FILTER_MIXER_MVOL - Filter mixer master volume.
#define YM2149_FILTER_MIXER_MVOL_ADDR 0x218
#define YM2149_FILTER_MIXER_MVOL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer master volume.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_mvol_t;

// FILTER_MIXER_MVOL.VALUE - Filter mixer master volume.
#define YM2149_FILTER_MIXER_MVOL_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_MVOL_VALUE_LSB 0
#define YM2149_FILTER_MIXER_MVOL_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_MVOL_VALUE_RESET 0x0

// FILTER_MIXER_INV - Filter mixer channel inverter
#define YM2149_FILTER_MIXER_INV_ADDR 0x21c
#define YM2149_FILTER_MIXER_INV_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t INVA : 1; // Invert PSG0 channel A.
    uint32_t INVB : 1; // Invert PSG0 channel B.
    uint32_t INVC : 1; // Invert PSG0 channel C.
    uint32_t INVD : 1; // Invert PSG1 channel A.
    uint32_t INVE : 1; // Invert PSG1 channel B.
    uint32_t INVF : 1; // Invert PSG1 channel C.
    uint32_t : 26; // reserved
  };
} ym2149_filter_mixer_inv_t;

// FILTER_MIXER_INV.INVA - Invert PSG0 channel A.
#define YM2149_FILTER_MIXER_INV_INVA_WIDTH 1
#define YM2149_FILTER_MIXER_INV_INVA_LSB 0
#define YM2149_FILTER_MIXER_INV_INVA_MASK 0x1
#define YM2149_FILTER_MIXER_INV_INVA_RESET 0x0

// FILTER_MIXER_INV.INVB - Invert PSG0 channel B.
#define YM2149_FILTER_MIXER_INV_INVB_WIDTH 1
#define YM2149_FILTER_MIXER_INV_INVB_LSB 1
#define YM2149_FILTER_MIXER_INV_INVB_MASK 0x2
#define YM2149_FILTER_MIXER_INV_INVB_RESET 0x0

// FILTER_MIXER_INV.INVC - Invert PSG0 channel C.
#define YM2149_FILTER_MIXER_INV_INVC_WIDTH 1
#define YM2149_FILTER_MIXER_INV_INVC_LSB 2
#define YM2149_FILTER_MIXER_INV_INVC_MASK 0x4
#define YM2149_FILTER_MIXER_INV_INVC_RESET 0x0

// FILTER_MIXER_INV.INVD - Invert PSG1 channel A.
#define YM2149_FILTER_MIXER_INV_INVD_WIDTH 1
#define YM2149_FILTER_MIXER_INV_INVD_LSB 3
#define YM2149_FILTER_MIXER_INV_INVD_MASK 0x8
#define YM2149_FILTER_MIXER_INV_INVD_RESET 0x0

// FILTER_MIXER_INV.INVE - Invert PSG1 channel B.
#define YM2149_FILTER_MIXER_INV_INVE_WIDTH 1
#define YM2149_FILTER_MIXER_INV_INVE_LSB 4
#define YM2149_FILTER_MIXER_INV_INVE_MASK 0x10
#define YM2149_FILTER_MIXER_INV_INVE_RESET 0x0

// FILTER_MIXER_INV.INVF - Invert PSG1 channel C.
#define YM2149_FILTER_MIXER_INV_INVF_WIDTH 1
#define YM2149_FILTER_MIXER_INV_INVF_LSB 5
#define YM2149_FILTER_MIXER_INV_INVF_MASK 0x20
#define YM2149_FILTER_MIXER_INV_INVF_RESET 0x0

// FILTER_MIXER_BASS - Filter mixer bass level.
#define YM2149_FILTER_MIXER_BASS_ADDR 0x220
#define YM2149_FILTER_MIXER_BASS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer bass level.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_bass_t;

// FILTER_MIXER_BASS.VALUE - Filter mixer bass level.
#define YM2149_FILTER_MIXER_BASS_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_BASS_VALUE_LSB 0
#define YM2149_FILTER_MIXER_BASS_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_BASS_VALUE_RESET 0x0

// FILTER_MIXER_TREBLE - Filter mixer treble level.
#define YM2149_FILTER_MIXER_TREBLE_ADDR 0x224
#define YM2149_FILTER_MIXER_TREBLE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // Filter mixer treble level.
    uint32_t : 24; // reserved
  };
} ym2149_filter_mixer_treble_t;

// FILTER_MIXER_TREBLE.VALUE - Filter mixer treble level.
#define YM2149_FILTER_MIXER_TREBLE_VALUE_WIDTH 8
#define YM2149_FILTER_MIXER_TREBLE_VALUE_LSB 0
#define YM2149_FILTER_MIXER_TREBLE_VALUE_MASK 0xff
#define YM2149_FILTER_MIXER_TREBLE_VALUE_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t PSG0_CHA_TONE_PERIOD_FINE; // Channel A fine tone period adjustment.
        __IO ym2149_psg0_cha_tone_period_fine_t PSG0_CHA_TONE_PERIOD_FINE_bf; // Bit access for PSG0_CHA_TONE_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG0_CHA_TONE_PERIOD_COARSE; // Channel A coarse tone period adjustment.
        __IO ym2149_psg0_cha_tone_period_coarse_t PSG0_CHA_TONE_PERIOD_COARSE_bf; // Bit access for PSG0_CHA_TONE_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG0_CHB_TONE_PERIOD_FINE; // Channel B fine tone period adjustment.
        __IO ym2149_psg0_chb_tone_period_fine_t PSG0_CHB_TONE_PERIOD_FINE_bf; // Bit access for PSG0_CHB_TONE_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG0_CHB_TONE_PERIOD_COARSE; // Channel B coarse tone period adjustment.
        __IO ym2149_psg0_chb_tone_period_coarse_t PSG0_CHB_TONE_PERIOD_COARSE_bf; // Bit access for PSG0_CHB_TONE_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG0_CHC_TONE_PERIOD_FINE; // Channel C fine tone period adjustment.
        __IO ym2149_psg0_chc_tone_period_fine_t PSG0_CHC_TONE_PERIOD_FINE_bf; // Bit access for PSG0_CHC_TONE_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG0_CHC_TONE_PERIOD_COARSE; // Channel C coarse tone period adjustment.
        __IO ym2149_psg0_chc_tone_period_coarse_t PSG0_CHC_TONE_PERIOD_COARSE_bf; // Bit access for PSG0_CHC_TONE_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG0_NOISE_PERIOD; // Noise period.
        __IO ym2149_psg0_noise_period_t PSG0_NOISE_PERIOD_bf; // Bit access for PSG0_NOISE_PERIOD register
    };
    union {
        __IO uint32_t PSG0_DISABLE; // Mixer settings.
        __IO ym2149_psg0_disable_t PSG0_DISABLE_bf; // Bit access for PSG0_DISABLE register
    };
    union {
        __IO uint32_t PSG0_CHA_LVL; // Channel A level control.
        __IO ym2149_psg0_cha_lvl_t PSG0_CHA_LVL_bf; // Bit access for PSG0_CHA_LVL register
    };
    union {
        __IO uint32_t PSG0_CHB_LVL; // Channel B level control.
        __IO ym2149_psg0_chb_lvl_t PSG0_CHB_LVL_bf; // Bit access for PSG0_CHB_LVL register
    };
    union {
        __IO uint32_t PSG0_CHC_LVL; // Channel C level control.
        __IO ym2149_psg0_chc_lvl_t PSG0_CHC_LVL_bf; // Bit access for PSG0_CHC_LVL register
    };
    union {
        __IO uint32_t PSG0_ENV_PERIOD_FINE; // Envelope frequency fine adjustment.
        __IO ym2149_psg0_env_period_fine_t PSG0_ENV_PERIOD_FINE_bf; // Bit access for PSG0_ENV_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG0_ENV_PERIOD_COARSE; // Envelope frequeny coarse adjustment.
        __IO ym2149_psg0_env_period_coarse_t PSG0_ENV_PERIOD_COARSE_bf; // Bit access for PSG0_ENV_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG0_ENV_SHAPE; // Envelope shape.
        __IO ym2149_psg0_env_shape_t PSG0_ENV_SHAPE_bf; // Bit access for PSG0_ENV_SHAPE register
    };
    __IO uint32_t RESERVED0[2];
    union {
        __IO uint32_t PSG1_CHA_TONE_PERIOD_FINE; // Channel A fine tone period adjustment.
        __IO ym2149_psg1_cha_tone_period_fine_t PSG1_CHA_TONE_PERIOD_FINE_bf; // Bit access for PSG1_CHA_TONE_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG1_CHA_TONE_PERIOD_COARSE; // Channel A coarse tone period adjustment.
        __IO ym2149_psg1_cha_tone_period_coarse_t PSG1_CHA_TONE_PERIOD_COARSE_bf; // Bit access for PSG1_CHA_TONE_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG1_CHB_TONE_PERIOD_FINE; // Channel B fine tone period adjustment.
        __IO ym2149_psg1_chb_tone_period_fine_t PSG1_CHB_TONE_PERIOD_FINE_bf; // Bit access for PSG1_CHB_TONE_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG1_CHB_TONE_PERIOD_COARSE; // Channel B coarse tone period adjustment.
        __IO ym2149_psg1_chb_tone_period_coarse_t PSG1_CHB_TONE_PERIOD_COARSE_bf; // Bit access for PSG1_CHB_TONE_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG1_CHC_TONE_PERIOD_FINE; // Channel C fine tone period adjustment.
        __IO ym2149_psg1_chc_tone_period_fine_t PSG1_CHC_TONE_PERIOD_FINE_bf; // Bit access for PSG1_CHC_TONE_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG1_CHC_TONE_PERIOD_COARSE; // Channel C coarse tone period adjustment.
        __IO ym2149_psg1_chc_tone_period_coarse_t PSG1_CHC_TONE_PERIOD_COARSE_bf; // Bit access for PSG1_CHC_TONE_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG1_NOISE_PERIOD; // Noise period.
        __IO ym2149_psg1_noise_period_t PSG1_NOISE_PERIOD_bf; // Bit access for PSG1_NOISE_PERIOD register
    };
    union {
        __IO uint32_t PSG1_DISABLE; // Mixer settings.
        __IO ym2149_psg1_disable_t PSG1_DISABLE_bf; // Bit access for PSG1_DISABLE register
    };
    union {
        __IO uint32_t PSG1_CHA_LVL; // Channel A level control.
        __IO ym2149_psg1_cha_lvl_t PSG1_CHA_LVL_bf; // Bit access for PSG1_CHA_LVL register
    };
    union {
        __IO uint32_t PSG1_CHB_LVL; // Channel B level control.
        __IO ym2149_psg1_chb_lvl_t PSG1_CHB_LVL_bf; // Bit access for PSG1_CHB_LVL register
    };
    union {
        __IO uint32_t PSG1_CHC_LVL; // Channel C level control.
        __IO ym2149_psg1_chc_lvl_t PSG1_CHC_LVL_bf; // Bit access for PSG1_CHC_LVL register
    };
    union {
        __IO uint32_t PSG1_ENV_PERIOD_FINE; // Envelope frequency fine adjustment.
        __IO ym2149_psg1_env_period_fine_t PSG1_ENV_PERIOD_FINE_bf; // Bit access for PSG1_ENV_PERIOD_FINE register
    };
    union {
        __IO uint32_t PSG1_ENV_PERIOD_COARSE; // Envelope frequeny coarse adjustment.
        __IO ym2149_psg1_env_period_coarse_t PSG1_ENV_PERIOD_COARSE_bf; // Bit access for PSG1_ENV_PERIOD_COARSE register
    };
    union {
        __IO uint32_t PSG1_ENV_SHAPE; // Envelope shape.
        __IO ym2149_psg1_env_shape_t PSG1_ENV_SHAPE_bf; // Bit access for PSG1_ENV_SHAPE register
    };
    __IO uint32_t RESERVED1[98];
    union {
        __IO uint32_t FILTER_MIXER_VOLA; // Filter mixer PSG0 channel A volume.
        __IO ym2149_filter_mixer_vola_t FILTER_MIXER_VOLA_bf; // Bit access for FILTER_MIXER_VOLA register
    };
    union {
        __IO uint32_t FILTER_MIXER_VOLB; // Filter mixer PSG0 channel B volume.
        __IO ym2149_filter_mixer_volb_t FILTER_MIXER_VOLB_bf; // Bit access for FILTER_MIXER_VOLB register
    };
    union {
        __IO uint32_t FILTER_MIXER_VOLC; // Filter mixer PSG0 channel C volume.
        __IO ym2149_filter_mixer_volc_t FILTER_MIXER_VOLC_bf; // Bit access for FILTER_MIXER_VOLC register
    };
    union {
        __IO uint32_t FILTER_MIXER_VOLD; // Filter mixer PSG1 channel A volume.
        __IO ym2149_filter_mixer_vold_t FILTER_MIXER_VOLD_bf; // Bit access for FILTER_MIXER_VOLD register
    };
    union {
        __IO uint32_t FILTER_MIXER_VOLE; // Filter mixer PSG1 channel B volume.
        __IO ym2149_filter_mixer_vole_t FILTER_MIXER_VOLE_bf; // Bit access for FILTER_MIXER_VOLE register
    };
    union {
        __IO uint32_t FILTER_MIXER_VOLF; // Filter mixer PSG1 channel C volume.
        __IO ym2149_filter_mixer_volf_t FILTER_MIXER_VOLF_bf; // Bit access for FILTER_MIXER_VOLF register
    };
    union {
        __IO uint32_t FILTER_MIXER_MVOL; // Filter mixer master volume.
        __IO ym2149_filter_mixer_mvol_t FILTER_MIXER_MVOL_bf; // Bit access for FILTER_MIXER_MVOL register
    };
    union {
        __IO uint32_t FILTER_MIXER_INV; // Filter mixer channel inverter
        __IO ym2149_filter_mixer_inv_t FILTER_MIXER_INV_bf; // Bit access for FILTER_MIXER_INV register
    };
    union {
        __IO uint32_t FILTER_MIXER_BASS; // Filter mixer bass level.
        __IO ym2149_filter_mixer_bass_t FILTER_MIXER_BASS_bf; // Bit access for FILTER_MIXER_BASS register
    };
    union {
        __IO uint32_t FILTER_MIXER_TREBLE; // Filter mixer treble level.
        __IO ym2149_filter_mixer_treble_t FILTER_MIXER_TREBLE_bf; // Bit access for FILTER_MIXER_TREBLE register
    };
} ym2149_t;

#define YM2149 ((ym2149_t*)(YM2149_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __YM2149_REGS_H */
