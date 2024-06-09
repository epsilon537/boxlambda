#include "YmMusicBL.h"
#include "utils.h"
#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "ff.h"
#include "platform.h"
#include <stdio.h>
#include <string.h>
#include "ym2149_sys_regs.h"
#include "sdram.h"

//This test program loads a .ym music file from the SD card and plays it back on one of the
//two YM2149 PSGs using the STsound library.

#define YM2149_SYS_BASE 0x10001000
#define YM2149_PSG_0 (YM2149_SYS_BASE+PSG0_CHA_TONE_PERIOD_FINE_OFFSET*4)
#define YM2149_PSG_1 (YM2149_SYS_BASE+PSG1_CHA_TONE_PERIOD_FINE_OFFSET*4)

#define DISK_DEV_NUM "0:"
#define STR_ROOT_DIRECTORY ""

const char *root_dir_name = STR_ROOT_DIRECTORY;
const char *ym_file_name = STR_ROOT_DIRECTORY "ancool1.ym";

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

unsigned mval = 10;
unsigned bass = 25;
unsigned treble = 128;

#ifdef __cplusplus
extern "C"
{
#endif

	//_init is executed by picolibc startup code before main().
	void _init(void)
	{
		// Set up UART and tie stdio to it.
		uart_init(&uart0, (volatile void *)PLATFORM_UART_BASE);
		uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
		set_stdio_to_uart(&uart0);

		mcycle_start();
	}

	//_exit is executed by the picolibc exit function.
	// An implementation has to be provided to be able to user assert().
	void _exit(int status)
	{
		while (1);
	}

#ifdef __cplusplus
}
#endif

FILINFO Finfo;
#if USE_LFN
char Lfname[512];
#endif

//This function lists the given path's directory contents.
static FRESULT scan_files(
	const char *path /* Pointer to the path name working buffer */
)
{
	DIR dirs;
	FRESULT res;
	BYTE i;
	char *fn;

	if ((res = f_opendir(&dirs, path)) == FR_OK)
	{
		i = strlen(path);
		while (((res = f_readdir(&dirs, &Finfo)) == FR_OK) && Finfo.fname[0])
		{
			if (Finfo.fname[0] == '.')
				continue;
#if USE_LFN
			fn = *Finfo.lfname ? Finfo.lfname : Finfo.fname;
#else
			fn = Finfo.fname;
#endif
			printf("%s/%s\n", path, fn);
		}
	}

	return res;
}

int main(void)
{
	FRESULT res;
	static DIR dirs;

	gpio_init(&gpio0, (volatile void *)PLATFORM_GPIO0_BASE);
	gpio_set_direction(&gpio0, 0x0000000F); // 4 inputs, 4 outputs

	gpio_init(&gpio1, (volatile void *)PLATFORM_GPIO1_BASE);
	gpio_set_direction(&gpio1, 0x00000000); // 4 inputs

	/*sdram_init() is provided by the Litex code base.*/
	if (sdram_init()) {
		printf("SDRAM init OK.\n");
	}
	else {
		printf("SDRAM init failed!\n");
		while(1);
	}
	//Program the audio mixer registers
	unsigned addrs[] = {FILTER_MIXER_VOLA_OFFSET, FILTER_MIXER_VOLB_OFFSET, FILTER_MIXER_VOLC_OFFSET,
						FILTER_MIXER_VOLD_OFFSET, FILTER_MIXER_VOLE_OFFSET, FILTER_MIXER_VOLF_OFFSET,
						FILTER_MIXER_MVOL_OFFSET, FILTER_MIXER_INV_OFFSET, FILTER_MIXER_BASS_OFFSET, FILTER_MIXER_TREB_OFFSET};
	unsigned vals[] = {50, 50, 50,
					   50, 50, 50,
					   mval, 0, bass, treble};

	for (int ii = 0; ii < (sizeof(addrs) / sizeof(addrs[0])); ii++)
	{
		ym2149_sys_reg_wr(addrs[ii], vals[ii]);
	}

	/* Declare these as static to avoid stack usage.
	 * They each contain an array of maximum sector size.
	 */
	static FATFS fs;

	printf("Mounting...\n");
	/* Clear file system object */
	memset(&fs, 0, sizeof(FATFS));
	res = f_mount(&fs, "", 1);
	if (res != FR_OK)
	{
		printf("FatFS mount error! %d\n", res);
		return -1;
	}

	printf("Listing directory contents...\n");
	scan_files(root_dir_name);

	//Instantiate two PSGs. Based on switch 3 setting, the music will play on one or the other.
	static CYmMusic cyMusic_psg_0((volatile ymint *)YM2149_PSG_0);
	static CYmMusic cyMusic_psg_1((volatile ymint *)YM2149_PSG_1);
	CYmMusic* cyMusicp = 0;

	if (gpio_get_input(&gpio0) & 0x80)
	{
		printf("Switching to PSG_0\n");
		cyMusicp = &cyMusic_psg_0;
	}
	else
	{
		printf("Switching to PSG_1\n");
		cyMusicp = &cyMusic_psg_1;
	}

	printf("Loading YM file: %s ...\n", ym_file_name);
	if (!cyMusicp->load(ym_file_name))
	{
		printf("CyMusic load failed!\n");
		return -1;
	}

	cyMusicp->setLoopMode(true);
	printf("Starting playback...\n");

	cyMusicp->play();

	uint32_t prevTimeClocks = mcycle_get32();
	uint32_t curTimeClocks;

	while (1)
	{
		curTimeClocks = mcycle_get32();

		//Every 20ms...
		if (cc2us(curTimeClocks - prevTimeClocks) >= 20000)
		{
			prevTimeClocks = curTimeClocks;
			cyMusicp->player(); //'tick' the music player.

			//Set switch 0, 1 or 2 to select volume, bass or treble control.
    		//Then press buttons 0 or 1 to increase/decrease.
			if (gpio_get_input(&gpio0) & 0x10)
			{
				if (gpio_get_input(&gpio1) & 0x01)
				{
					if (mval < 255)
						++mval;

					ym2149_sys_reg_wr(FILTER_MIXER_MVOL_OFFSET, mval);
					printf("mval: %d\n", mval);
				}

				if (gpio_get_input(&gpio1) & 0x02)
				{
					if (mval > 0)
						--mval;

					ym2149_sys_reg_wr(FILTER_MIXER_MVOL_OFFSET, mval);
					printf("mval: %d\n", mval);
				}
			}

			if (gpio_get_input(&gpio0) & 0x20)
			{
				if (gpio_get_input(&gpio1) & 0x01)
				{
					if (bass < 63)
						++bass;

					ym2149_sys_reg_wr(FILTER_MIXER_BASS_OFFSET, bass);
					printf("bass: %d\n", bass);
				}

				if (gpio_get_input(&gpio1) & 0x02)
				{
					if (bass > 1)
						--bass;

					ym2149_sys_reg_wr(FILTER_MIXER_BASS_OFFSET, bass);
					printf("bass: %d\n", bass);
				}
			}

			if (gpio_get_input(&gpio0) & 0x40)
			{
				if (gpio_get_input(&gpio1) & 0x01)
				{
					if (treble < 255)
						++treble;

					ym2149_sys_reg_wr(FILTER_MIXER_TREB_OFFSET, treble);
					printf("treble: %d\n", treble);
				}

				if (gpio_get_input(&gpio1) & 0x02)
				{
					if (treble > 0)
						--treble;

					ym2149_sys_reg_wr(FILTER_MIXER_TREB_OFFSET, treble);
					printf("treble: %d\n", treble);
				}
			}
		}
	}

	return 0;
}
