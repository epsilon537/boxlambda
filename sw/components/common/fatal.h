#ifndef FATAL_H
#define FATAL_H

#include <stdio.h>

#define die(...) {printf(__VA_ARGS__); while (1);}

#endif /*FATAL_H*/
