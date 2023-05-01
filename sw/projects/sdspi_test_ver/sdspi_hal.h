#ifndef SDSPI_HAL_H
#define SDSPI_HAL_H

#define SDSPI_BASE      0x10000020
#define	SDSPI_CMD_ADDR	(SDSPI_BASE+(0<<2))
#define	SDSPI_DATA_ADDR	(SDSPI_BASE+(1<<2))
#define	SDSPI_FIFO_A	(SDSPI_BASE+(2<<2))
#define	SDSPI_FIFO_B	(SDSPI_BASE+(3<<2))

#define	READAUX	0x80
#define	SETAUX	0xc0

#define	SDSPI_CMD		0x000040
#define	SDSPI_ACMD		(SDSPI_CMD + 55)
#define	SDSPI_READREG		0x000200
#define	SDSPI_FIFO_OP		0x000800
#define	SDSPI_WRITEOP		0x000c00
#define	SDSPI_FIFO_ID		0x001000
#define	SDSPI_BUSY		0x004000
#define	SDSPI_ERROR		0x008000
#define	SDSPI_CLEARERR		0x008000
#define	SDSPI_REMOVED		0x040000
#define	SDSPI_PRESENTN		0x080000
#define	SDSPI_RESET		0x100000
#define	SDSPI_WATCHDOG		0x200000
#define	SDSPI_GO_IDLE		((SDSPI_REMOVED|SDSPI_CLEARERR|SDSPI_CMD)+0)
#define	SDSPI_READ_SECTOR	((SDSPI_CMD|SDSPI_CLEARERR|SDSPI_FIFO_OP)+17)
#define	SDSPI_WRITE_SECTOR	((SDSPI_CMD|SDSPI_CLEARERR|SDSPI_WRITEOP)+24)

unsigned sdspi_read_aux(void);
unsigned sdspi_set_aux(unsigned aux);
void sdspi_wait_while_busy(void);
unsigned sdspi_sdcmd(int cmd, unsigned arg);
unsigned sdspi_read(int cmd, unsigned arg, int ln, unsigned *data);
unsigned sdspi_write(int cmd, unsigned arg, int ln, unsigned *data);
unsigned sdspi_read_ocr(void);
unsigned sdspi_read_csd(unsigned *data);
unsigned sdspi_read_cid(unsigned *data);

#endif //SDSPI_HAL_H
