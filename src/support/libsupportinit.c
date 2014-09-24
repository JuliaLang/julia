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

static int isInitialized = 0;

void libsupport_init(void)
{
    if (!isInitialized) {
#ifdef _OS_WINDOWS_
        SetConsoleCP(1252); // ANSI Latin1; Western European (Windows)
#endif
        setlocale(LC_ALL, ""); // set to user locale
        setlocale(LC_NUMERIC, "C"); // use locale-independent numeric formats

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
