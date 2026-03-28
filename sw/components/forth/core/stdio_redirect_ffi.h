#ifndef STDIO_REDIRECT_FFI_H
#define STDIO_REDIRECT_FFI_H

#include "forth.h"

#ifdef __cplusplus
extern "C" {
#endif

// Set up C stdio to Forth emit/key redirection.
void stdio_redirect_ffi_init();

#ifdef __cplusplus
}
#endif
#endif //SDTIO_REDIRECT_FFI_H


