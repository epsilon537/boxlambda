# YM2149 Register map

Created with [Corsair](https://github.com/esynr3z/corsair) v1.0.4.

## Conventions

| Access mode | Description               |
| :---------- | :------------------------ |
| rw          | Read and Write            |
| rw1c        | Read and Write 1 to Clear |
| rw1s        | Read and Write 1 to Set   |
| ro          | Read Only                 |
| roc         | Read Only to Clear        |
| roll        | Read Only / Latch Low     |
| rolh        | Read Only / Latch High    |
| wo          | Write only                |
| wosc        | Write Only / Self Clear   |

## Register map summary

Base address: 0x10001000

| Name                     | Address    | Description |
| :---                     | :---       | :---        |
| [PSG0_CHA_TONE_PERIOD_FINE](#psg0_cha_tone_period_fine) | 0x00000000 | Channel A fine tone period adjustment. |
| [PSG0_CHA_TONE_PERIOD_COARSE](#psg0_cha_tone_period_coarse) | 0x00000004 | Channel A coarse tone period adjustment. |
| [PSG0_CHB_TONE_PERIOD_FINE](#psg0_chb_tone_period_fine) | 0x00000008 | Channel B fine tone period adjustment. |
| [PSG0_CHB_TONE_PERIOD_COARSE](#psg0_chb_tone_period_coarse) | 0x0000000c | Channel B coarse tone period adjustment. |
| [PSG0_CHC_TONE_PERIOD_FINE](#psg0_chc_tone_period_fine) | 0x00000010 | Channel C fine tone period adjustment. |
| [PSG0_CHC_TONE_PERIOD_COARSE](#psg0_chc_tone_period_coarse) | 0x00000014 | Channel C coarse tone period adjustment. |
| [PSG0_NOISE_PERIOD](#psg0_noise_period) | 0x00000018 | Noise period. |
| [PSG0_DISABLE](#psg0_disable) | 0x0000001c | Mixer settings. |
| [PSG0_CHA_LVL](#psg0_cha_lvl) | 0x00000020 | Channel A level control. |
| [PSG0_CHB_LVL](#psg0_chb_lvl) | 0x00000024 | Channel B level control. |
| [PSG0_CHC_LVL](#psg0_chc_lvl) | 0x00000028 | Channel C level control. |
| [PSG0_ENV_PERIOD_FINE](#psg0_env_period_fine) | 0x0000002c | Envelope frequency fine adjustment. |
| [PSG0_ENV_PERIOD_COARSE](#psg0_env_period_coarse) | 0x00000030 | Envelope frequeny coarse adjustment. |
| [PSG0_ENV_SHAPE](#psg0_env_shape) | 0x00000034 | Envelope shape. |
| [PSG1_CHA_TONE_PERIOD_FINE](#psg1_cha_tone_period_fine) | 0x00000040 | Channel A fine tone period adjustment. |
| [PSG1_CHA_TONE_PERIOD_COARSE](#psg1_cha_tone_period_coarse) | 0x00000044 | Channel A coarse tone period adjustment. |
| [PSG1_CHB_TONE_PERIOD_FINE](#psg1_chb_tone_period_fine) | 0x00000048 | Channel B fine tone period adjustment. |
| [PSG1_CHB_TONE_PERIOD_COARSE](#psg1_chb_tone_period_coarse) | 0x0000004c | Channel B coarse tone period adjustment. |
| [PSG1_CHC_TONE_PERIOD_FINE](#psg1_chc_tone_period_fine) | 0x00000050 | Channel C fine tone period adjustment. |
| [PSG1_CHC_TONE_PERIOD_COARSE](#psg1_chc_tone_period_coarse) | 0x00000054 | Channel C coarse tone period adjustment. |
| [PSG1_NOISE_PERIOD](#psg1_noise_period) | 0x00000058 | Noise period. |
| [PSG1_DISABLE](#psg1_disable) | 0x0000005c | Mixer settings. |
| [PSG1_CHA_LVL](#psg1_cha_lvl) | 0x00000060 | Channel A level control. |
| [PSG1_CHB_LVL](#psg1_chb_lvl) | 0x00000064 | Channel B level control. |
| [PSG1_CHC_LVL](#psg1_chc_lvl) | 0x00000068 | Channel C level control. |
| [PSG1_ENV_PERIOD_FINE](#psg1_env_period_fine) | 0x0000006c | Envelope frequency fine adjustment. |
| [PSG1_ENV_PERIOD_COARSE](#psg1_env_period_coarse) | 0x00000070 | Envelope frequeny coarse adjustment. |
| [PSG1_ENV_SHAPE](#psg1_env_shape) | 0x00000074 | Envelope shape. |
| [FILTER_MIXER_VOLA](#filter_mixer_vola) | 0x00000200 | Filter mixer PSG0 channel A volume. |
| [FILTER_MIXER_VOLB](#filter_mixer_volb) | 0x00000204 | Filter mixer PSG0 channel B volume. |
| [FILTER_MIXER_VOLC](#filter_mixer_volc) | 0x00000208 | Filter mixer PSG0 channel C volume. |
| [FILTER_MIXER_VOLD](#filter_mixer_vold) | 0x0000020c | Filter mixer PSG1 channel A volume. |
| [FILTER_MIXER_VOLE](#filter_mixer_vole) | 0x00000210 | Filter mixer PSG1 channel B volume. |
| [FILTER_MIXER_VOLF](#filter_mixer_volf) | 0x00000214 | Filter mixer PSG1 channel C volume. |
| [FILTER_MIXER_MVOL](#filter_mixer_mvol) | 0x00000218 | Filter mixer master volume. |
| [FILTER_MIXER_INV](#filter_mixer_inv) | 0x0000021c | Filter mixer channel inverter |
| [FILTER_MIXER_BASS](#filter_mixer_bass) | 0x00000220 | Filter mixer bass level. |
| [FILTER_MIXER_TREBLE](#filter_mixer_treble) | 0x00000224 | Filter mixer treble level. |

## PSG0_CHA_TONE_PERIOD_FINE

Channel A fine tone period adjustment.

Address offset: 0x00000000

Reset value: 0x00000000

![psg0_cha_tone_period_fine](md_img/psg0_cha_tone_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Channel A fine tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_CHA_TONE_PERIOD_COARSE

Channel A coarse tone period adjustment.

Address offset: 0x00000004

Reset value: 0x00000000

![psg0_cha_tone_period_coarse](md_img/psg0_cha_tone_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Channel A coarse tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_CHB_TONE_PERIOD_FINE

Channel B fine tone period adjustment.

Address offset: 0x00000008

Reset value: 0x00000000

![psg0_chb_tone_period_fine](md_img/psg0_chb_tone_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Channel B fine tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_CHB_TONE_PERIOD_COARSE

Channel B coarse tone period adjustment.

Address offset: 0x0000000c

Reset value: 0x00000000

![psg0_chb_tone_period_coarse](md_img/psg0_chb_tone_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Channel B coarse tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_CHC_TONE_PERIOD_FINE

Channel C fine tone period adjustment.

Address offset: 0x00000010

Reset value: 0x00000000

![psg0_chc_tone_period_fine](md_img/psg0_chc_tone_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Channel C fine tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_CHC_TONE_PERIOD_COARSE

Channel C coarse tone period adjustment.

Address offset: 0x00000014

Reset value: 0x00000000

![psg0_chc_tone_period_coarse](md_img/psg0_chc_tone_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Channel C coarse tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_NOISE_PERIOD

Noise period.

Address offset: 0x00000018

Reset value: 0x00000000

![psg0_noise_period](md_img/psg0_noise_period.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| VALUE            | 4:0    | rw              | 0x0        | Noise period. |

Back to [Register map](#register-map-summary).

## PSG0_DISABLE

Mixer settings.

Address offset: 0x0000001c

Reset value: 0x00000000

![psg0_disable](md_img/psg0_disable.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:6   | -               | 0x000000   | Reserved |
| NOISE_C          | 5      | rw              | 0x0        | Channel C noise disable. |
| NOISE_B          | 4      | rw              | 0x0        | Channel B noise disable. |
| NOISE_A          | 3      | rw              | 0x0        | Channel A noise disable. |
| TONE_C           | 2      | rw              | 0x0        | Channel C tone disable. |
| TONE_B           | 1      | rw              | 0x0        | Channel B tone disable. |
| TONE_A           | 0      | rw              | 0x0        | Channel A tone disable. |

Back to [Register map](#register-map-summary).

## PSG0_CHA_LVL

Channel A level control.

Address offset: 0x00000020

Reset value: 0x00000000

![psg0_cha_lvl](md_img/psg0_cha_lvl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| MODE             | 4      | rw              | 0x0        | Mode. |
| LVL              | 3:0    | rw              | 0x0        | Level selection. |

Back to [Register map](#register-map-summary).

## PSG0_CHB_LVL

Channel B level control.

Address offset: 0x00000024

Reset value: 0x00000000

![psg0_chb_lvl](md_img/psg0_chb_lvl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| MODE             | 4      | rw              | 0x0        | Mode. |
| LVL              | 3:0    | rw              | 0x0        | Level selection. |

Back to [Register map](#register-map-summary).

## PSG0_CHC_LVL

Channel C level control.

Address offset: 0x00000028

Reset value: 0x00000000

![psg0_chc_lvl](md_img/psg0_chc_lvl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| MODE             | 4      | rw              | 0x0        | Mode. |
| LVL              | 3:0    | rw              | 0x0        | Level selection. |

Back to [Register map](#register-map-summary).

## PSG0_ENV_PERIOD_FINE

Envelope frequency fine adjustment.

Address offset: 0x0000002c

Reset value: 0x00000000

![psg0_env_period_fine](md_img/psg0_env_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Envelope frequency fine adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_ENV_PERIOD_COARSE

Envelope frequeny coarse adjustment.

Address offset: 0x00000030

Reset value: 0x00000000

![psg0_env_period_coarse](md_img/psg0_env_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Envelope frequency coarse adjustment. |

Back to [Register map](#register-map-summary).

## PSG0_ENV_SHAPE

Envelope shape.

Address offset: 0x00000034

Reset value: 0x00000000

![psg0_env_shape](md_img/psg0_env_shape.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Envelope shape. |

Back to [Register map](#register-map-summary).

## PSG1_CHA_TONE_PERIOD_FINE

Channel A fine tone period adjustment.

Address offset: 0x00000040

Reset value: 0x00000000

![psg1_cha_tone_period_fine](md_img/psg1_cha_tone_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Channel A fine tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_CHA_TONE_PERIOD_COARSE

Channel A coarse tone period adjustment.

Address offset: 0x00000044

Reset value: 0x00000000

![psg1_cha_tone_period_coarse](md_img/psg1_cha_tone_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Channel A coarse tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_CHB_TONE_PERIOD_FINE

Channel B fine tone period adjustment.

Address offset: 0x00000048

Reset value: 0x00000000

![psg1_chb_tone_period_fine](md_img/psg1_chb_tone_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Channel B fine tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_CHB_TONE_PERIOD_COARSE

Channel B coarse tone period adjustment.

Address offset: 0x0000004c

Reset value: 0x00000000

![psg1_chb_tone_period_coarse](md_img/psg1_chb_tone_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Channel B coarse tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_CHC_TONE_PERIOD_FINE

Channel C fine tone period adjustment.

Address offset: 0x00000050

Reset value: 0x00000000

![psg1_chc_tone_period_fine](md_img/psg1_chc_tone_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Channel C fine tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_CHC_TONE_PERIOD_COARSE

Channel C coarse tone period adjustment.

Address offset: 0x00000054

Reset value: 0x00000000

![psg1_chc_tone_period_coarse](md_img/psg1_chc_tone_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Channel C coarse tone period adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_NOISE_PERIOD

Noise period.

Address offset: 0x00000058

Reset value: 0x00000000

![psg1_noise_period](md_img/psg1_noise_period.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| VALUE            | 4:0    | rw              | 0x0        | Noise period. |

Back to [Register map](#register-map-summary).

## PSG1_DISABLE

Mixer settings.

Address offset: 0x0000005c

Reset value: 0x00000000

![psg1_disable](md_img/psg1_disable.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:6   | -               | 0x000000   | Reserved |
| NOISE_C          | 5      | rw              | 0x0        | Channel C noise disable. |
| NOISE_B          | 4      | rw              | 0x0        | Channel B noise disable. |
| NOISE_A          | 3      | rw              | 0x0        | Channel A noise disable. |
| TONE_C           | 2      | rw              | 0x0        | Channel C tone disable. |
| TONE_B           | 1      | rw              | 0x0        | Channel B tone disable. |
| TONE_A           | 0      | rw              | 0x0        | Channel A tone disable. |

Back to [Register map](#register-map-summary).

## PSG1_CHA_LVL

Channel A level control.

Address offset: 0x00000060

Reset value: 0x00000000

![psg1_cha_lvl](md_img/psg1_cha_lvl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| MODE             | 4      | rw              | 0x0        | Mode. |
| LVL              | 3:0    | rw              | 0x0        | Level selection. |

Back to [Register map](#register-map-summary).

## PSG1_CHB_LVL

Channel B level control.

Address offset: 0x00000064

Reset value: 0x00000000

![psg1_chb_lvl](md_img/psg1_chb_lvl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| MODE             | 4      | rw              | 0x0        | Mode. |
| LVL              | 3:0    | rw              | 0x0        | Level selection. |

Back to [Register map](#register-map-summary).

## PSG1_CHC_LVL

Channel C level control.

Address offset: 0x00000068

Reset value: 0x00000000

![psg1_chc_lvl](md_img/psg1_chc_lvl.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:5   | -               | 0x000000   | Reserved |
| MODE             | 4      | rw              | 0x0        | Mode. |
| LVL              | 3:0    | rw              | 0x0        | Level selection. |

Back to [Register map](#register-map-summary).

## PSG1_ENV_PERIOD_FINE

Envelope frequency fine adjustment.

Address offset: 0x0000006c

Reset value: 0x00000000

![psg1_env_period_fine](md_img/psg1_env_period_fine.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Envelope frequency fine adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_ENV_PERIOD_COARSE

Envelope frequeny coarse adjustment.

Address offset: 0x00000070

Reset value: 0x00000000

![psg1_env_period_coarse](md_img/psg1_env_period_coarse.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Envelope frequency coarse adjustment. |

Back to [Register map](#register-map-summary).

## PSG1_ENV_SHAPE

Envelope shape.

Address offset: 0x00000074

Reset value: 0x00000000

![psg1_env_shape](md_img/psg1_env_shape.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:4   | -               | 0x0000000  | Reserved |
| VALUE            | 3:0    | rw              | 0x0        | Envelope shape. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_VOLA

Filter mixer PSG0 channel A volume.

Address offset: 0x00000200

Reset value: 0x00000000

![filter_mixer_vola](md_img/filter_mixer_vola.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer PSG0 channel A volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_VOLB

Filter mixer PSG0 channel B volume.

Address offset: 0x00000204

Reset value: 0x00000000

![filter_mixer_volb](md_img/filter_mixer_volb.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer PSG0 channel B volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_VOLC

Filter mixer PSG0 channel C volume.

Address offset: 0x00000208

Reset value: 0x00000000

![filter_mixer_volc](md_img/filter_mixer_volc.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer PSG0 channel C volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_VOLD

Filter mixer PSG1 channel A volume.

Address offset: 0x0000020c

Reset value: 0x00000000

![filter_mixer_vold](md_img/filter_mixer_vold.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer PSG1 channel A volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_VOLE

Filter mixer PSG1 channel B volume.

Address offset: 0x00000210

Reset value: 0x00000000

![filter_mixer_vole](md_img/filter_mixer_vole.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer PSG1 channel B volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_VOLF

Filter mixer PSG1 channel C volume.

Address offset: 0x00000214

Reset value: 0x00000000

![filter_mixer_volf](md_img/filter_mixer_volf.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer PSG1 channel C volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_MVOL

Filter mixer master volume.

Address offset: 0x00000218

Reset value: 0x00000000

![filter_mixer_mvol](md_img/filter_mixer_mvol.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer master volume. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_INV

Filter mixer channel inverter

Address offset: 0x0000021c

Reset value: 0x00000000

![filter_mixer_inv](md_img/filter_mixer_inv.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:6   | -               | 0x000000   | Reserved |
| INVF             | 5      | rw              | 0x0        | Invert PSG1 channel C. |
| INVE             | 4      | rw              | 0x0        | Invert PSG1 channel B. |
| INVD             | 3      | rw              | 0x0        | Invert PSG1 channel A. |
| INVC             | 2      | rw              | 0x0        | Invert PSG0 channel C. |
| INVB             | 1      | rw              | 0x0        | Invert PSG0 channel B. |
| INVA             | 0      | rw              | 0x0        | Invert PSG0 channel A. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_BASS

Filter mixer bass level.

Address offset: 0x00000220

Reset value: 0x00000000

![filter_mixer_bass](md_img/filter_mixer_bass.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer bass level. |

Back to [Register map](#register-map-summary).

## FILTER_MIXER_TREBLE

Filter mixer treble level.

Address offset: 0x00000224

Reset value: 0x00000000

![filter_mixer_treble](md_img/filter_mixer_treble.svg)

| Name             | Bits   | Mode            | Reset      | Description |
| :---             | :---   | :---            | :---       | :---        |
| -                | 31:8   | -               | 0x000000   | Reserved |
| VALUE            | 7:0    | rw              | 0x00       | Filter mixer treble level. |

Back to [Register map](#register-map-summary).
