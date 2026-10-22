#ifndef FORTH_FAST_BOOT_H
#define FORTH_FAST_BOOT_H

// Forth Fast Boot support, saving Forth's state in an image and restoring it
// later (rather than compiling everything from scratch at boot time).

// Initialize the module. This will register the forth_save_state() function
// with Forth.
void forth_fastboot_init();

// Restore a previously saved Forth image.
void forth_fastboot_load(const char *boxkern_forth_image_start,
                         const char *boxkern_forth_image_end);

#endif /*FORTH_FAST_BOOT_H*/
