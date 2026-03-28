#ifndef STDIO_STREAM_H
#define STDIO_STREAM_H

#include <stdio.h>

// export stdin and stdout stream objects to allow stdio redirection.
extern FILE stdin_stream;
extern FILE stdout_stream;

#endif /*STDIO_STREAM*/
