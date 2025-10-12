/*
 * BoxLambda port of Mecrisp Cube FS module.
 * Ruben Lysens / Epsilon537.
 *
 * Original header:
 *
 * fs.h
 *
 *  Created on: 17.02.2020
 *      Author: psi
 */

#ifndef INC_FS_H_
#define INC_FS_H_

#include "ff.h"

extern const char FS_Version[];
// extern	uint32_t **ZweitDictionaryPointer;

// extern int EvaluateState;


void FS_init(void);

int FS_FIL_size(void);
int FS_FATFS_size(void);
int FS_DIR_size(void);
int FS_FILINFO_size(void);
int FS_FILINFO_fsize(void);
int FS_FILINFO_fdate(void);
int FS_FILINFO_ftime(void);
int FS_FILINFO_fattrib(void);
int FS_FILINFO_fname(void);
int FS_FILINFO_altname(void);

int FS_f_eof(FIL* fp);
FSIZE_t FS_f_size(FIL* fp);
int FS_f_error(FIL* fp);
int FS_getc(FIL* fp);

void FS_included ();
void FS_cat      ();
void FS_ls       ();
void FS_cd       ();
void FS_pwd      ();
void FS_mkdir    ();
void FS_rm       ();
void FS_chmod    ();
void FS_touch    ();
void FS_mv       ();
void FS_cp       ();
void FS_df       ();
void FS_date     ();
void FS_mount    ();
void FS_umount   ();
void FS_mkfs     ();

void FS_evaluate (uint8_t *str, int count);
// void FS_catch_evaluate (uint8_t *str, int count);
void FS_type     (uint8_t *str, int count);
void FS_cr       ();
void FS_token    (uint8_t **str, int *count);
void FS_accept   (uint8_t *str, int *count);


#endif /* INC_FS_H_ */
