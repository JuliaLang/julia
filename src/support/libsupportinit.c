#include <locale.h>
#include <math.h>
#include "libsupport.h"

#ifdef __cplusplus
extern "C" {
#endif

double D_PNAN;
double D_NNAN;
double D_PINF;
double D_NINF;

DLLEXPORT int locale_is_utf8;
static int isInitialized = 0;

void libsupport_init(void)
{
    if (!isInitialized) {
        locale_is_utf8 = u8_is_locale_utf8(setlocale(LC_ALL, ""));
        setlocale(LC_NUMERIC, "C");

        ios_init_stdstreams();

        D_PNAN = +NAN;
        D_NNAN = -NAN;
        D_PINF = +INFINITY;
        D_NINF = -INFINITY;
        isInitialized=1;
    }
}

#ifdef __cplusplus
}
#endif
