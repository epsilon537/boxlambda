#include "YmMusicBL.h"
#include "utils.h"
#include "stdio_to_uart.h"
#include "uart.h"
#include "ff.h"
#include "platform.h"
#include <stdio.h>
#include <string.h>

#define YM2149_SYS_BASE 0x10001000

#define DISK_DEV_NUM       "0:"
#define STR_ROOT_DIRECTORY ""

const char *root_dir_name = STR_ROOT_DIRECTORY;
const char *ym_file_name = STR_ROOT_DIRECTORY "ancool1.ym";

static struct uart uart0;

#ifdef __cplusplus
extern "C" {
#endif

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);

  mtime_start();
}

//_exit is executed by the picolibc exit function. 
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

#ifdef __cplusplus
}
#endif

FILINFO Finfo;
#if USE_LFN
char Lfname[512];
#endif

static
FRESULT scan_files (
	const char* path		/* Pointer to the path name working buffer */
)
{
	DIR dirs;
	FRESULT res;
	BYTE i;
	char *fn;

	if ((res = f_opendir(&dirs, path)) == FR_OK) {
		i = strlen(path);
		while (((res = f_readdir(&dirs, &Finfo)) == FR_OK) && Finfo.fname[0]) {
			if (Finfo.fname[0] == '.') continue;
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

void ym2149_sys_reg_wr(unsigned reg_offset, unsigned val) {
  ((unsigned volatile*)(YM2149_SYS_BASE))[reg_offset] = val;
}

int main(void) {
    static CYmMusic cyMusic((volatile ymint*)YM2149_SYS_BASE);

    FRESULT res;
	static DIR dirs;

	unsigned addrs[] = { 128,129, 130,131,132,133, 134, 135, 136, 137 } ;
  	unsigned vals[]  = {  50, 50,  50, 64, 64, 64, 10,   0,  25, 25 } ;

	for (int ii=0; ii<(sizeof(addrs)/sizeof(addrs[0])); ii++) {
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
	if (res != FR_OK) {
		printf("FatFS mount error! %d\n", res);
    	return -1;
	}

	printf("Listing directory contents...\n");
	scan_files(root_dir_name);

	printf("Loading YM file: %s ...\n", ym_file_name);
    if (!cyMusic.load(ym_file_name)) {
		printf("CyMusic load failed!\n");
    	return -1;
	}

	printf("Starting playback...\n");
	cyMusic.play();

	uint32_t prevTimeClocks = mtime_get32();
	uint32_t curTimeClocks;

	while (1) {
		curTimeClocks = mtime_get32();

		if (cc2us(curTimeClocks-prevTimeClocks) >= 20000) {
			prevTimeClocks = curTimeClocks;
			cyMusic.player();
		}
	}

    return 0;
}