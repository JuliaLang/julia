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

ENTRY(jl_setjmp)
    movq    0(%rsp),%rdx
    movq    $0,0(%rcx)
    movq    %rbx,8(%rcx)
    movq    %rsp,16(%rcx)
    movq    %rbp,24(%rcx)
    movq    %rsi,32(%rcx)
    movq    %rdi,40(%rcx)
    movq    %r12,48(%rcx)
    movq    %r13,56(%rcx)
    movq    %r14,64(%rcx)
    movq    %r15,72(%rcx)
    movq    %rdx,80(%rcx)
    movq    $0,88(%rcx)
    movaps  %xmm6,96(%rcx)
    movaps  %xmm7,112(%rcx)
    movaps  %xmm8,128(%rcx)
    movaps  %xmm9,144(%rcx)
    movaps  %xmm10,160(%rcx)
    movaps  %xmm11,176(%rcx)
    movaps  %xmm12,192(%rcx)
    movaps  %xmm13,208(%rcx)
    movaps  %xmm14,224(%rcx)
    movaps  %xmm15,240(%rcx)

    xorq    %rax,%rax
    ret
END(jl_setjmp)

