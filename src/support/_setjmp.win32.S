/*  $NetBSD: _setjmp.S,v 1.8 2005/10/05 20:18:12 christos Exp $ */

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *  from: @(#)_setjmp.s 5.1 (Berkeley) 4/23/90
 */

/*#include <machine/asm.h>
#if defined(LIBC_SCCS)
    RCSID("$NetBSD: _setjmp.S,v 1.8 2005/10/05 20:18:12 christos Exp $")
#endif
*/
#include "../../deps/openlibm/i387/bsd_asm.h"

/*
 * C library -- _setjmp, _longjmp
 *
 *  _longjmp(a,v)
 * will generate a "return(v)" from the last call to
 *  _setjmp(a)
 * by restoring registers from the stack.
 * The previous signal state is NOT restored.
 */

ENTRY(jl_setjmp)
    movl    4(%esp),%eax
    movl    0(%esp),%edx
    movl    %ebp, 0(%eax)       /* rta */
    movl    %ebx, 4(%eax)
    movl    %edi, 8(%eax)
    movl    %esi,12(%eax)
    movl    %esp,16(%eax)
    movl    %edx,20(%eax)
    xorl    %eax,%eax
    ret
END(jl_setjmp)

