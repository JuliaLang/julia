/*
 * Copyright (c) 2004 Darren Tucker.
 *
 * Based originally on asprintf.c from OpenBSD:
 * Copyright (c) 1997 Todd C. Miller <Todd.Miller AT courtesan.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* Include vasprintf() if not on your OS. */
#ifndef HAVE_VASPRINTF

#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef VA_COPY
# ifdef HAVE_VA_COPY
#  define VA_COPY(dest, src) va_copy(dest, src)
# else
#  ifdef HAVE___VA_COPY
#   define VA_COPY(dest, src) __va_copy(dest, src)
#  else
#   define VA_COPY(dest, src) (dest) = (src)
#  endif
# endif
#endif

#define INIT_SZ 128

#ifdef __cplusplus
extern "C" {
#endif

int
vasprintf(char **str, const char *fmt, va_list ap)
{
        int ret = -1;
        va_list ap2;
        char *string, *newstr;
        size_t len;

        VA_COPY(ap2, ap);
        if ((string = (char*)malloc(INIT_SZ)) == NULL)
                goto fail;

        ret = vsnprintf(string, INIT_SZ, fmt, ap2);
        if (ret >= 0 && ret < INIT_SZ) { /* succeeded with initial alloc */
                *str = string;
        } else if (ret == INT_MAX || ret < 0) { /* Bad length */
                goto fail;
        } else {        /* bigger than initial, realloc allowing for nul */
                len = (size_t)ret + 1;
                if ((newstr = (char*)realloc(string, len)) == NULL) {
                        free(string);
                        goto fail;
                } else {
                        va_end(ap2);
                        VA_COPY(ap2, ap);
                        ret = vsnprintf(newstr, len, fmt, ap2);
                        if (ret >= 0 && (size_t)ret < len) {
                                *str = newstr;
                        } else { /* failed with realloc'ed string, give up */
                                free(newstr);
                                goto fail;
                        }
                }
        }
        va_end(ap2);
        return (ret);

fail:
        *str = NULL;
        errno = ENOMEM;
        va_end(ap2);
        return (-1);
}
#endif

/* Include asprintf() if not on your OS. */
#ifndef HAVE_ASPRINTF
int asprintf(char **str, const char *fmt, ...)
{
        va_list ap;
        int ret;

        *str = NULL;
        va_start(ap, fmt);
        ret = vasprintf(str, fmt, ap);
        va_end(ap);

        return ret;
}
#endif

#ifdef __cplusplus
}
#endif
