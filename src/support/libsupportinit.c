// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <locale.h>
#include "libsupport.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char *jl_strchrnul(const char *s, int c)
{
    char *p = strchr(s, c);
    if (p)
        return p;
    return s + strlen(s);
}

void libsupport_init(void)
{
    static int isInitialized = 0;
    if (!isInitialized) {
        ios_init_stdstreams();
        isInitialized = 1;

        // adopt the user's locale for most formatting
        setlocale(LC_ALL, "");
        // but use locale-independent numeric formats (for parsing)
        setlocale(LC_NUMERIC, "C");
        // and try to specify ASCII or UTF-8 (preferred) for our Libc and Cstring functions
        char *ctype = setlocale(LC_CTYPE, NULL);
        if (ctype) {
            size_t codeset = jl_strchrnul(ctype, '.') - ctype;
            if (strncmp(ctype + codeset, ".UTF-8", strlen(".UTF-8")) == 0 ||
                strncmp(ctype + codeset, ".utf-8", strlen(".utf-8")) == 0 ||
                strncmp(ctype + codeset, ".utf8", strlen(".utf8")) == 0)
                return; // already UTF-8
            ctype = (char*)memcpy(malloc_s(codeset + sizeof(".UTF-8")), ctype, codeset);
            strcpy(ctype + codeset, ".UTF-8");
        }
        setlocale(LC_CTYPE, "C"); // ASCII
#ifndef _OS_WINDOWS_
        if (setlocale(LC_CTYPE, "C.UTF-8") == NULL && // Linux/FreeBSD name
            setlocale(LC_CTYPE, "en_US.UTF-8") == NULL && // Common name
            setlocale(LC_CTYPE, "UTF-8") == NULL && // Apple name
            (ctype == NULL || setlocale(LC_CTYPE, ctype) == NULL)) { // attempt to form it manually
            ios_puts("WARNING: failed to select UTF-8 encoding, using ASCII\n", ios_stderr);
        }
#endif
        if (ctype)
            free(ctype);
    }
}

#ifdef __cplusplus
}
#endif
