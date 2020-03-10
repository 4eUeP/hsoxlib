#include <stdlib.h>
#include <sox.h>

LSX_RETURN_VALID void *lsx_calloc(size_t n, size_t size);

#define array_length(a) (sizeof(a)/sizeof(a[0]))

/* ------------------------------ Maths stuff -------------------------------*/

#include <math.h>

#ifdef min
#undef min
#endif
#define min(a, b) ((a) <= (b) ? (a) : (b))

#ifdef max
#undef max
#endif
#define max(a, b) ((a) >= (b) ? (a) : (b))

#define dB_to_linear(x) exp((x) * M_LN10 * 0.05)
#define linear_to_dB(x) (log10(x) * 20)
