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

/*
 * C library -- _setjmp, _longjmp
 *
 *  _longjmp(a,v)
 * will generate a "return(v)" from the last call to
 *  _setjmp(a)
 * by restoring registers from the stack.
 * The previous signal state is NOT restored.
 *  _swapcontext(a, b)
 * Store the current context in a and resume in context b
 *
 * TODO: save/restore floating point control state
 * and reset avx state
 * and update fs:[0xEOC] to contain the address of the stack
 */

#define CNAME jl_setjmp
#include "ENTRY.i387.h"
    mov    eax,DWORD PTR [esp+4] // arg 1
    mov    edx,DWORD PTR [esp+0] // rta
    mov    DWORD PTR [eax+0],ebp
    mov    DWORD PTR [eax+4],ebx
    mov    DWORD PTR [eax+8],edi
    mov    DWORD PTR [eax+12],esi
    mov    DWORD PTR [eax+16],esp
    mov    DWORD PTR [eax+20],edx // eip
    mov    edx,DWORD PTR fs:[0] // seh registration
    mov    DWORD PTR [eax+24],edx
    xor    eax,eax # return 0
    ret
#include "END.h"


#define CNAME jl_longjmp
#include "ENTRY.i387.h"
    mov    edx,DWORD PTR [esp+4] // arg 1
    mov    eax,DWORD PTR [esp+8] // arg 2
    mov    ebp,DWORD PTR [edx+24] // seh registration
    mov    ecx,DWORD PTR [edx+20] // eip
    mov    esp,DWORD PTR [edx+16]
    mov    esi,DWORD PTR [edx+12]
    mov    edi,DWORD PTR [edx+8]
    mov    ebx,DWORD PTR [edx+4]
    mov    DWORD PTR fs:[0],ebp
    mov    ebp,DWORD PTR [edx+0]
    mov    DWORD PTR [esp],ecx
    test   eax,eax
    jne    a
    inc    eax
a:  ret    // jmp ecx
#include "END.h"


#define CNAME jl_swapcontext
#include "ENTRY.i387.h"
    mov    eax,DWORD PTR [esp+4]
    // save stack registers
    mov    edx,DWORD PTR fs:[8] // stack top (low)
    mov    ecx,DWORD PTR fs:[4] // stack bottom (high)
    mov    DWORD PTR [eax+0],edx // sp
    sub    ecx,edx
    mov    DWORD PTR [eax+4],ecx // ssize
    add    eax,8
    // save uc_mcontext
    mov    edx,DWORD PTR [esp+0]
    mov    ecx,DWORD PTR fs:[0]
    mov    DWORD PTR [eax+0],ebp
    mov    DWORD PTR [eax+4],ebx
    mov    DWORD PTR [eax+8],edi
    mov    DWORD PTR [eax+12],esi
    mov    DWORD PTR [eax+16],esp
    mov    DWORD PTR [eax+20],edx
    mov    DWORD PTR [eax+24],ecx
    add    esp,4
    jmp    _jl_setcontext
#include "END.h"


#define CNAME jl_setcontext
#include "ENTRY.i387.h"
    mov    eax,DWORD PTR [esp+4]
    // restore stack registers
    mov    edx,DWORD PTR [eax+0]
    mov    ecx,DWORD PTR [eax+4]
    mov    DWORD PTR fs:[8],edx // stack top (low)
    add    ecx,edx
    mov    DWORD PTR fs:[4],ecx // stack bottom (high)
    add    eax,8
    // restore uc_mcontext
    mov    ebp,DWORD PTR [eax+24]
    mov    ecx,DWORD PTR [eax+20]
    mov    esp,DWORD PTR [eax+16]
    mov    esi,DWORD PTR [eax+12]
    mov    edi,DWORD PTR [eax+8]
    mov    ebx,DWORD PTR [eax+4]
    mov    DWORD PTR fs:[0],ebp
    mov    ebp,DWORD PTR [eax+0]
    mov    DWORD PTR [esp],ecx
    xor    eax,eax # return 0
    inc    eax # HACK: return 1
    ret
#include "END.h"


#define CNAME __readgs
#include "ENTRY.i387.h"
    mov eax,gs
    ret
#include "END.h"


#define CNAME __readgsdword
#include "ENTRY.i387.h"
    mov eax,DWORD PTR [esp+4]
    mov eax,gs:[eax]
    ret
#include "END.h"
