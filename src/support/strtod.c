#define _GNU_SOURCE
#include "libsupport.h"
#include <stdlib.h>
#include <locale.h>

#if defined(__APPLE__) || defined(__FreeBSD__)
#include <xlocale.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(_OS_WINDOWS_)
// This code path should be used for systems that support the strtod_l function

// Cache locale object
static int c_locale_initialized = 0;
static locale_t c_locale;

locale_t get_c_locale(void)
{
    if (!c_locale_initialized) {
        c_locale_initialized = 1;
        c_locale = newlocale(LC_ALL_MASK, "C", NULL);
    }
    return c_locale;
}

JL_DLLEXPORT double jl_strtod_c(const char *nptr, char **endptr)
{
    return strtod_l(nptr, endptr, get_c_locale());
}

JL_DLLEXPORT float jl_strtof_c(const char *nptr, char **endptr)
{
    return strtof_l(nptr, endptr, get_c_locale());
}


#else
// This code path should be used for systems that do not support the strtod_l function
// Currently this is MinGW/Windows

// The following code is derived from the Python function _PyOS_ascii_strtod
// see http://hg.python.org/cpython/file/default/Python/pystrtod.c
//
// Copyright © 2001-2014 Python Software Foundation; All Rights Reserved
//
// The following modifications have been made:
// - Leading spaces are ignored
// - Parsing of hex floats is supported in the derived version
// - Python functions for tolower, isdigit and malloc have been replaced by the respective
//   C stdlib functions

#include <ctype.h>

int case_insensitive_match(const char *s, const char *t)
{
    while (*t && tolower(*s) == *t) {
        s++;
        t++;
    }
    return *t ? 0 : 1;
}

double parse_inf_or_nan(const char *p, char **endptr)
{
    double retval;
    const char *s;
    int negate = 0;

    s = p;
    if (*s == '-') {
        negate = 1;
        s++;
    }
    else if (*s == '+') {
        s++;
    }
    if (case_insensitive_match(s, "inf")) {
        s += 3;
        if (case_insensitive_match(s, "inity"))
            s += 5;
        retval = negate ? -D_PINF : D_PINF;
    }
    else if (case_insensitive_match(s, "nan")) {
        s += 3;
        retval = negate ? -D_PNAN : D_PNAN;
    }
    else {
        s = p;
        retval = -1.0;
    }
    *endptr = (char *)s;
    return retval;
}


JL_DLLEXPORT double jl_strtod_c(const char *nptr, char **endptr)
{
    char *fail_pos;
    double val;
    struct lconv *locale_data;
    const char *decimal_point;
    size_t decimal_point_len;
    const char *p, *decimal_point_pos;
    const char *end = NULL; /* Silence gcc */
    const char *digits_pos = NULL;
    int negate = 0;

    fail_pos = NULL;

    locale_data = localeconv();
    decimal_point = locale_data->decimal_point;
    decimal_point_len = strlen(decimal_point);

    decimal_point_pos = NULL;

    /* Parse infinities and nans */
    val = parse_inf_or_nan(nptr, endptr);
    if (*endptr != nptr)
        return val;

    /* Set errno to zero, so that we can distinguish zero results
       and underflows */
    errno = 0;

    /* We process the optional sign manually, then pass the remainder to
       the system strtod.  This ensures that the result of an underflow
       has the correct sign.  */
    p = nptr;

    /* parse leading spaces */
    while (isspace((unsigned char)*p)) {
        p++;
    }

    /* Process leading sign, if present */
    if (*p == '-') {
        negate = 1;
        p++;
    }
    else if (*p == '+') {
        p++;
    }

    /* This code path is used for hex floats */
    if (*p == '0' && (*(p+1) == 'x' || *(p+1) == 'X')) {
        digits_pos = p;
        p += 2;
        /* Check that what's left begins with a digit or decimal point */
        if (!isxdigit(*p) && *p != '.')
            goto invalid_string;


        if (decimal_point[0] != '.' || decimal_point[1] != 0) {
            /* Look for a '.' in the input; if present, it'll need to be
               swapped for the current locale's decimal point before we
               call strtod.  On the other hand, if we find the current
               locale's decimal point then the input is invalid. */
            while (isxdigit(*p))
                p++;

            if (*p == '.') {
                decimal_point_pos = p++;

                /* locate end of number */
                while (isxdigit(*p))
                    p++;

                if (*p == 'p' || *p == 'P')
                    p++;
                if (*p == '+' || *p == '-')
                    p++;
                while (isdigit(*p))
                    p++;
                end = p;
            }
            else if (strncmp(p, decimal_point, decimal_point_len) == 0)
                goto invalid_string;
            /* For the other cases, we need not convert the decimal point */
        }
    }
    else {
        /* Check that what's left begins with a digit or decimal point */
        if (!isdigit(*p) && *p != '.')
            goto invalid_string;

        digits_pos = p;
        if (decimal_point[0] != '.' || decimal_point[1] != 0) {
            /* Look for a '.' in the input; if present, it'll need to be
               swapped for the current locale's decimal point before we
               call strtod.  On the other hand, if we find the current
               locale's decimal point then the input is invalid. */
            while (isdigit(*p))
                p++;

            if (*p == '.') {
                decimal_point_pos = p++;

                /* locate end of number */
                while (isdigit(*p))
                    p++;

                if (*p == 'e' || *p == 'E')
                    p++;
                if (*p == '+' || *p == '-')
                    p++;
                while (isdigit(*p))
                    p++;
                end = p;
            }
            else if (strncmp(p, decimal_point, decimal_point_len) == 0)
                goto invalid_string;
            /* For the other cases, we need not convert the decimal point */
        }
    }

    if (decimal_point_pos) {
        char *copy, *c;
        /* Create a copy of the input, with the '.' converted to the
           locale-specific decimal point */
        copy = (char *)malloc(end - digits_pos +
                                    1 + decimal_point_len);
        if (copy == NULL) {
            *endptr = (char *)nptr;
            errno = ENOMEM;
            return val;
        }

        c = copy;
        memcpy(c, digits_pos, decimal_point_pos - digits_pos);
        c += decimal_point_pos - digits_pos;
        memcpy(c, decimal_point, decimal_point_len);
        c += decimal_point_len;
        memcpy(c, decimal_point_pos + 1,
               end - (decimal_point_pos + 1));
        c += end - (decimal_point_pos + 1);
        *c = 0;

        val = strtod(copy, &fail_pos);

        if (fail_pos)
        {
            if (fail_pos > decimal_point_pos)
                fail_pos = (char *)digits_pos +
                    (fail_pos - copy) -
                    (decimal_point_len - 1);
            else
                fail_pos = (char *)digits_pos +
                    (fail_pos - copy);
        }

        free(copy);
    }
    else {
        val = strtod(digits_pos, &fail_pos);
    }

    if (fail_pos == digits_pos)
        goto invalid_string;

    if (negate && fail_pos != nptr)
        val = -val;
    *endptr = fail_pos;

    return val;

invalid_string:
    *endptr = (char*)nptr;
    errno = EINVAL;
    return -1.0;
}


JL_DLLEXPORT float jl_strtof_c(const char *nptr, char **endptr)
{
    return (float) jl_strtod_c(nptr, endptr);
}

#endif

#ifdef __cplusplus
}
#endif
