/*-
 * Copyright (c) 2003 Mike Barcroft <mike@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD: src/lib/msun/src/s_signbit.c,v 1.1.30.1.6.1 2010/12/21 17:09:25 kensmith Exp $
 */

#include "fdlibm.h"

union IEEEf2bits {
    float f;
    struct {
#if _BYTE_ORDER == _LITTLE_ENDIAN
        unsigned int man :23;
        unsigned int exp :8;
        unsigned int sign :1;
#else /* _BIG_ENDIAN */
        unsigned int sign :1;
        unsigned int exp :8;
        unsigned int man :23;
#endif
    } bits;
};

#define DBL_MANH_SIZE 20
#define DBL_MANL_SIZE 32

union IEEEd2bits {
    double d;
    struct {
#if _BYTE_ORDER == _LITTLE_ENDIAN
        unsigned int manl :32;
        unsigned int manh :20;
        unsigned int exp :11;
        unsigned int sign :1;
#else /* _BIG_ENDIAN */
        unsigned int sign :1;
        unsigned int exp :11;
        unsigned int manh :20;
        unsigned int manl :32;
#endif
    } bits;
};

int
signbit(double d)
{
    union IEEEd2bits u;

    u.d = d;
    return (u.bits.sign);
}

int
signbitf(float f)
{
    union IEEEf2bits u;

    u.f = f;
    return (u.bits.sign);
}
