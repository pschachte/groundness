#include <stdio.h>
#ifdef SWI
#define IOSTREAM_REPLACES_STDIO 1
#undef sprintf
#undef vsprintf
#include <SWI-Stream.h>
#endif

#include "bryant.h"

/* prints out the bryant graph a */
extern void printOut(FILE *stream, node *a);
