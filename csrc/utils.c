#include <stdlib.h>
#include <sox.h>

static void *lsx_checkptr(void *ptr)
{
    if (!ptr) {
        lsx_fail("out of memory");
        exit(2);
    }

    return ptr;
}

void *lsx_calloc(size_t n, size_t size)
{
    return lsx_checkptr(calloc(n + !n, size + !size));
}
