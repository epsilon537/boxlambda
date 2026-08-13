#ifndef FORTH_FAST_BOOT_H
#define FORTH_FAST_BOOT_H

void forth_fastboot_init();

void forth_fastboot_load(const char *boxkern_forth_image_start,
                         const char *boxkern_forth_image_end);

#endif /*FORTH_FAST_BOOT_H*/
