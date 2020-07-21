// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <locale.h>
#include "libsupport.h"

#ifdef __cplusplus
extern "C" {
#endif

static int isInitialized = 0;

void libsupport_init(void)
{
    if (!isInitialized) {

        setlocale(LC_ALL, ""); // set to user locale
        setlocale(LC_NUMERIC, "C"); // use locale-independent numeric formats

        ios_init_stdstreams();

        isInitialized=1;
    }
}

#ifdef __cplusplus
}
#endif
