/*  $NetBSD: _setjmp.S,v 1.1 2001/06/19 00:25:02 fvdl Exp $ */

/*
 * Copyright (c) 2001 Wasabi Systems, Inc.
 * All rights reserved.
 *
 * Written by Frank van der Linden for Wasabi Systems, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed for the NetBSD Project by
 *      Wasabi Systems, Inc.
 * 4. The name of Wasabi Systems, Inc. may not be used to endorse
 *    or promote products derived from this software without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WASABI SYSTEMS, INC. ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL WASABI SYSTEMS, INC
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
#include <machine/asm.h>
#include <machine/setjmp.h>
#if defined(LIBC_SCCS)
    RCSID("$NetBSD: _setjmp.S,v 1.1 2001/06/19 00:25:02 fvdl Exp $")
#endif
*/
#include "../../deps/openlibm/amd64/bsd_asm.h"


/*
 * C library -- _setjmp, _longjmp
 *
 *  _longjmp(a,v)
 * will generate a "return(v)" from the last call to
 *  _setjmp(a)
 * by restoring registers from the stack.
 * The previous signal state is NOT restored.
 */

ENTRY(jl_longjmp)
    movq    8(%rcx),%rbx
    movq    16(%rcx),%rsp
    movq    24(%rcx),%rbp
    movq    32(%rcx),%rsi
    movq    40(%rcx),%rdi
    movq    48(%rcx),%r12
    movq    56(%rcx),%r13
    movq    64(%rcx),%r14
    movq    72(%rcx),%r15
    movq    80(%rcx),%r8
    movaps  96(%rcx),%xmm6
    movaps  112(%rcx),%xmm7
    movaps  128(%rcx),%xmm8
    movaps  144(%rcx),%xmm9
    movaps  160(%rcx),%xmm10
    movaps  176(%rcx),%xmm11
    movaps  192(%rcx),%xmm12
    movaps  208(%rcx),%xmm13
    movaps  224(%rcx),%xmm14
    movaps  240(%rcx),%xmm15
    
    movl    %edx,%eax
    testl   %eax,%eax
    jnz     1f
    incl    %eax
1:  movq    %r8,0(%rsp)
    ret
END(jl_longjmp)

